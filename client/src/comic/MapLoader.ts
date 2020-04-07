import isEqual from 'lodash/isEqual'
import groupBy from 'lodash/groupBy'
import throttle from 'lodash/throttle'
import sortBy from 'lodash/sortBy'
import RBush from 'rbush'
import {Emitter} from '@servie/events'

import type {Loc, ZLoc, BinLoc, ItemLoc, BinData, ZItemMap} from '../types'
import {APIClient} from '../APIClient'

const VIEW_QUANTIZE = 500
const VIEW_EXTRA_SCREENS = 2
const RENDER_EXTRA_SCREENS =  .5
const UPDATE_THROTTLE = 50

function setTimeoutAt(cb: () => void, expiresAt: number, minDelay: number = 0) {
  const delay = Math.max(minDelay, expiresAt - Date.now())
  return setTimeout(cb, delay)
}

function quantizeDown(x: number, unit: number) {
  return unit * Math.floor(x / unit)
}

function quantizeUp(x: number, unit: number) {
  return unit * Math.ceil(x / unit)
}

function mapLocWithin(outer: MapLoc, inner: MapLoc): boolean {
  return (
    outer.scale === inner.scale
    && outer.minX <= inner.minX
    && outer.maxX >= inner.maxX
    && outer.minY <= inner.minY
    && outer.maxY >= inner.maxY
  )
}

export class ViewCache {
  retryTime: number = 5000
  api: APIClient
  onExpire: () => void
  onUpdate: () => void
  immediateValue?: false | RBush<BinLoc>
  asyncValue?: Promise<false | RBush<BinLoc>>
  thresholdLoc?: MapLoc
  timeout?: number

  constructor({api, onExpire, onUpdate}: {api: APIClient, onExpire: () => void, onUpdate: () => void}) {
    this.api = api
    this.onExpire = onExpire
    this.onUpdate = onUpdate
  }

  async getCachedAndRefresh(loc: MapLoc) {
    const queuedRefresh = this.refresh(loc)

    if (!this.immediateValue) {
      await queuedRefresh
    }

    return this.immediateValue
  }

  async refresh(loc: MapLoc) {
    if (this.asyncValue === undefined || !this.thresholdLoc || !mapLocWithin(this.thresholdLoc, loc)) {
      this.asyncValue = this._load(loc)
      this.immediateValue = await this.asyncValue
      this.onUpdate?.()
    }
  }

  async _load(loc: MapLoc) {
    const width = loc.maxX - loc.minX
    const height = loc.maxY - loc.minY

    const threshold = VIEW_EXTRA_SCREENS * .5
    this.thresholdLoc = {
      minX: quantizeDown(loc.minX - threshold * width, VIEW_QUANTIZE),
      minY: quantizeDown(loc.minY - threshold * height, VIEW_QUANTIZE),
      maxX: quantizeUp(loc.maxX + threshold * width, VIEW_QUANTIZE),
      maxY: quantizeUp(loc.maxY + threshold * height, VIEW_QUANTIZE),
      scale: loc.scale,
    }

    const viewLoc = {
      minX: quantizeDown(loc.minX - VIEW_EXTRA_SCREENS * width, VIEW_QUANTIZE),
      minY: quantizeDown(loc.minY - VIEW_EXTRA_SCREENS * height, VIEW_QUANTIZE),
      maxX: quantizeUp(loc.maxX + VIEW_EXTRA_SCREENS * width, VIEW_QUANTIZE),
      maxY: quantizeUp(loc.maxY + VIEW_EXTRA_SCREENS * height, VIEW_QUANTIZE),
    }

    let result
    try {
      result = await this.api.fetchView(viewLoc)
    } catch (err) {
      console.error('Error loading view', viewLoc, err)
      this._scheduleUpdate(Date.now() + this.retryTime)
      return false
    }

    const {bins, expiresAt} = result
    const binBush: RBush<BinLoc> = new RBush()
    binBush.load(bins)

    this._scheduleUpdate(expiresAt)

    return binBush
  }

  _scheduleUpdate = (expiresAt: number) => {
    clearTimeout(this.timeout)
    this.timeout = setTimeoutAt(() => {
      this.asyncValue = undefined
      this.onExpire?.()
    }, expiresAt)
  }
}

type VisibleCacheValue = {
  renderLoc: Loc,
  visibleBins: Set<string>,
}

export class VisibleCache {
  extraScreens: number
  value?: VisibleCacheValue
  lastBinBush: RBush<BinLoc>
  thresholdLoc?: MapLoc

  constructor({extraScreens}: {extraScreens: number}) {
    this.extraScreens = extraScreens
  }

  get(loc: MapLoc, binBush: RBush<BinLoc>): [boolean, VisibleCacheValue] {
    let changed = false
    if (!this.value || binBush !== this.lastBinBush || !this.thresholdLoc || !mapLocWithin(this.thresholdLoc, loc)) {
      changed = true
      this.value = this._calculate(loc, binBush)
    }
    return [changed, this.value]
  }

  _calculate(loc: MapLoc, binBush: RBush<BinLoc>) {
    const width = loc.maxX - loc.minX
    const height = loc.maxY - loc.minY

    const threshold = this.extraScreens * .5
    this.thresholdLoc = {
      minX: loc.minX - threshold * width,
      minY: loc.minY - threshold * height,
      maxX: loc.maxX + threshold * width,
      maxY: loc.maxY + threshold * height,
      scale: loc.scale,
    }

    const renderLoc = {
      minX: Math.floor(loc.minX - this.extraScreens * width),
      minY: Math.floor(loc.minY - this.extraScreens * height),
      maxX: Math.floor(loc.maxX + this.extraScreens * width),
      maxY: Math.floor(loc.maxY + this.extraScreens * height),
    }

    const binLocs = binBush.search(renderLoc)
    const visibleBins = new Set(binLocs.map(b => b.id))

    this.lastBinBush = binBush

    return {renderLoc, visibleBins}
  }
}

type ZBushMap = Map<number, RBush<ItemLoc>>

export class BinItemsCache {
  retryTime: number = 5000
  api: APIClient
  onExpire: (id: string) => void
  asyncValueMap: Map<string, Promise<false | ZBushMap>> = new Map()
  zBushCache: Map<string, [Array<ItemLoc>, ZBushMap]> = new Map()
  timeoutMap: Map<string, number> = new Map()

  constructor({api, onExpire}: {api: APIClient, onExpire: (id: string) => void}) {
    this.api = api
    this.onExpire = onExpire
  }

  async get(id: string) {
    if (!this.asyncValueMap.has(id)) {
      this.asyncValueMap.set(id, this._load(id))
    }
    return await this.asyncValueMap.get(id)
  }

  async _load(id: string) {
    let result
    try {
      result = await this.api.fetchBin(id)
    } catch (err) {
      console.error('Error loading bin', id, err)
      this._scheduleUpdate(id, Date.now() + this.retryTime)
      return false
    }

    const {items, expiresAt} = result
    this._scheduleUpdate(id, expiresAt)
    return this._makeZBushMap(id, items)
  }

  storeBin(binData: BinData) {
    const {id, items, expiresAt} = binData
    const bush = this._makeZBushMap(id, items)
    this.asyncValueMap.set(id, Promise.resolve(bush))
    this._scheduleUpdate(id, expiresAt)
  }

  _makeZBushMap(id: string, items: Array<ItemLoc>) {
    if (this.zBushCache.has(id)) {
      const [oldItems, oldMap] = this.zBushCache.get(id)
      if (isEqual(oldItems, items)) {
        return oldMap
      }
    }

    const map = new Map()
    const zGroups = new Map()
    for (const item of items) {
      if (!zGroups.has(item.minZ)) {
        zGroups.set(item.minZ, [])
      }
      zGroups.get(item.minZ).push(item)
    }
    for (const [z, items] of zGroups.entries()) {
      const bush = new RBush()
      bush.load(items)
      map.set(z, bush)
    }

    this.zBushCache.set(id, [items, map])
    return map
  }

  _scheduleUpdate(id: string, expiresAt: number) {
    clearTimeout(this.timeoutMap.get(id))
    this.timeoutMap.set(
      id,
      setTimeoutAt(() => {
        this.asyncValueMap.delete(id)
        this.onExpire?.(id)
      }, expiresAt),
    )
  }
}

export interface RenderMetrics {
  minX: number,
  minY: number,
  offsetX: number,
  offsetY: number,
  width: number,
  height: number,
  viewBounds: Loc,
}

interface MapLoaderEvents {
  metrics: [RenderMetrics],
  update: [Array<ItemLoc>],
}

interface MapLoc extends Loc {
  scale: number,
}

export default class MapLoader {
  events: Emitter<MapLoaderEvents>
  loc: MapLoc

  viewCache: ViewCache
  visibleCache: VisibleCache
  itemsCache: BinItemsCache

  visibleBins: Set<string> = new Set()
  binItems: Map<string, ZBushMap> = new Map()
  imgs: Map<string, HTMLImageElement> = new Map()

  constructor(api: APIClient, {renderExtraScreens = RENDER_EXTRA_SCREENS}: {renderExtraScreens?: number} = {}) {
    this.events = new Emitter<MapLoaderEvents>()
    this.viewCache = new ViewCache({
      api,
      onExpire: this.handleViewExpire,
      onUpdate: this.handleViewUpdate,
    })
    this.visibleCache = new VisibleCache({extraScreens: renderExtraScreens})
    this.itemsCache = new BinItemsCache({
      api,
      onExpire: this.handleBinExpire,
    })
  }

  emitMetrics(loc: MapLoc, renderLoc: Loc, viewBounds: Loc) {
    this.events.emit('metrics', {
      minX: renderLoc.minX,
      minY: renderLoc.minY,
      offsetX: (renderLoc.minX - loc.minX) * loc.scale,
      offsetY: (loc.maxY - renderLoc.maxY) * loc.scale,
      width: (renderLoc.maxX - renderLoc.minX) * loc.scale,
      height: (renderLoc.maxY - renderLoc.minY) * loc.scale,
      viewBounds,
    })
  }

  emitUpdate = () => {
    this.events.emit('update', this.getItems())
  }

  queueUpdate = throttle(this.emitUpdate, UPDATE_THROTTLE, {leading: false})

  view = async (x: number, y: number, width: number, height: number, scale: number) => {
    this.loc = {
      minX: x,
      minY: y,
      maxX: x + width / scale,
      maxY: y + height / scale,
      scale,
    }
    this.loadView(this.loc)
  }

  checkCollision = (zloc: ZLoc): boolean => {
    for (const zBushMap of this.iterVisibleBins()) {
      if (zBushMap.get(zloc.minZ)?.collides(zloc)) {
        return true
      }
    }
    return false
  }

  getItemsAt = (loc: Loc): Array<ItemLoc> => {
    const result: Array<ItemLoc> = []
    for (const zBushMap of this.iterVisibleBins()) {
      for (const bush of zBushMap.values()) {
        result.splice(-1, 0, ...bush.search(loc))
      }
    }
    return result
  }

  async loadView(loc: MapLoc) {
    const binBush = await this.viewCache.getCachedAndRefresh(loc)
    if (!binBush) {
      this.visibleBins.clear()
      this.events.emit('metrics', null)
      return
    }

    const viewBounds = {
      minX: binBush.data.minX,
      minY: binBush.data.minY,
      maxX: binBush.data.maxX,
      maxY: binBush.data.maxY,
    }
    const [visibleChanged, {renderLoc, visibleBins}] = this.visibleCache.get(loc, binBush)
    this.emitMetrics(loc, renderLoc, viewBounds)

    if (visibleChanged) {
      this.visibleBins = visibleBins
      this.emitUpdate()
      for (const id of visibleBins) {
        this.loadBin(id)
      }
    }
  }

  async loadBin(id: string) {
    const items = await this.itemsCache.get(id)
    if (!items) {
      return
    }
    if (items !== this.binItems.get(id)) {
      this.binItems.set(id, items)
      this.queueUpdate()
    }
  }

  handleViewExpire = () => {
    this.viewCache.refresh(this.loc)
  }

  handleViewUpdate = () => {
    this.loadView(this.loc)
  }

  handleBinExpire = (id: string) => {
    if (this.visibleBins.has(id)) {
      this.loadBin(id)
    } else {
      this.binItems.delete(id)
    }
  }

  updateBins(binDatas: Array<BinData>) {
    for (const binData of binDatas) {
      this.itemsCache.storeBin(binData)
      this.loadBin(binData.id)
    }
  }

  *iterVisibleBins() {
    for (const binId of this.visibleBins) {
      const zBushMap = this.binItems.get(binId)
      if (!zBushMap) {
        continue
      }
      yield zBushMap
    }
  }

  getItems(): Array<ItemLoc> {
    const zLayers: Map<number, Array<ItemLoc>> = new Map()
    for (const zBushMap of this.iterVisibleBins()) {
      for (const [z, bush] of zBushMap.entries()) {
        if (!zLayers.has(z)) {
          zLayers.set(z, [])
        }
        zLayers.get(z).splice(-1, 0, ...bush.all())
      }
    }

    const items: Array<ItemLoc> = []
    const zs = sortBy([...zLayers.keys()], z => -z)
    for (const z of zs) {
      items.splice(-1, 0, ...zLayers.get(z))
    }
    return items
  }
}
