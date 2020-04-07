import random from 'lodash/random'

import type {ServerBinSpace, ServerItems, Loc, BinLoc, ItemLoc, BinData, ViewData, ServerClaim, ServerPlace, InventoryItem, ClaimResult, PlaceResult, ServerBinItemMap} from './types'

const DEFAULT_VIEW_MAX_AGE = 60000
const DEFAULT_BIN_MAX_AGE = 5000

function parseMaxAge(cacheControlHeader: string): number {
  if (!cacheControlHeader) {
    return null
  }
  const match = cacheControlHeader.match(/max-age=(\d+)/)
  if (!match) {
    return null
  }
  return Number(match[1])
}

function parseExpiresAt(cacheControlHeader: string, defaultAge: number): number {
  const maxAge = parseMaxAge(cacheControlHeader) ?? defaultAge
  return Date.now() + 1000 * maxAge
}

export interface APIClient {
  claimLoot: (session: string, path: string) => Promise<ClaimResult>,
  placeItem: (session: string, itemLoc: ItemLoc) => Promise<PlaceResult>,
  fetchView: (viewLoc: Loc) => Promise<ViewData>,
  fetchBin: (id: string) => Promise<BinData>,
}

export class MockAPIClient implements APIClient {
  jitter = 5
  binRange = 3
  itemCount = 10
  binSize = 500
  viewBins: Array<BinLoc>
  binDatas: Map<string, Omit<BinData, 'expiresAt'>>

  constructor() {
    const {jitter, binRange, itemCount, binSize} = this
    this.viewBins = []
    this.binDatas = new Map()
    const gridSize = binSize / itemCount
    let idx = 0
    for (let bx = -binRange; bx <= binRange; bx++) {
      for (let by = -binRange; by <= binRange; by++) {
        const id = String(idx++)
        this.viewBins.push({
          kind: 'bin',
          id,
          minX: bx * binSize,
          minY: by * binSize,
          maxX: (bx + 1) * binSize,
          maxY: (by + 1) * binSize,
        })

        const items: Array<ItemLoc> = []
        for (let i = 0; i < itemCount; i++) {
          items.push({
            kind: 'item',
            img: 'test.png',
            minX: bx * binSize + i * gridSize,
            minY: by * binSize + i * gridSize,
            maxX: bx * binSize + (i + 1) * gridSize,
            maxY: by * binSize + (i + 1) * gridSize,
            minZ: random(0, 4),
          })
        }

        this.binDatas.set(id, {
          id,
          items,
        })
      }
    }
  }

  async claimLoot(session: string, path: string): Promise<ClaimResult> {
    return {
      loot: [
        {
          img: 'cat.png',
          width: 100,
          height: 100,
          z: 0,
        },
        {
          img: 'hat.png',
          width: 200,
          height: 200,
          z: 1,
        },
      ],
      hints: [
        'Cats can have little a salami, as a treat.',
      ],
      session: 'opaquesessiondata',
    }
  }

  async placeItem(session: string, itemLoc: ItemLoc): Promise<PlaceResult> {
    return {
      placed: true,
      loot: [
        {
          img: 'bat.png',
          width: 120,
          height: 120,
          z: 0,
        },
      ],
      hints: [
        'Bats can also have little a salami, as a treat.',
      ],
      binDatas: [],
      session: 'opaquesessiondata',
    }
  }

  async fetchView(viewLoc: Loc): Promise<ViewData> {
    console.debug('fetch view', viewLoc)
    return {
      expiresAt: Date.now() + 10000,
      bins: this.viewBins,
    }
  }

  async fetchBin(id: string): Promise<BinData> {
    console.debug('fetch bin', id)
    const {items} = this.binDatas.get(id)
    return {
      id,
      expiresAt: Date.now() + 1000,
      items: items.map(({maxX, maxY, ...rest}) => ({
        maxX: maxX + random(this.jitter),
        maxY: maxY + random(this.jitter),
        ...rest,
      }))
    }
  }
}

export class MapleClient implements APIClient {
  serverURL: string

  constructor(serverURL: string) {
    this.serverURL = serverURL
  }

  _readServerLoot(loot: ServerItems): Array<InventoryItem> {
    return loot.map(({img, aabb: {min, max}}) => ({
      img,
      width: max[0] - min[0],
      height: max[1] - min[1],
      z: min[2],
    }))
  }

  _readServerItems(items: ServerItems): Array<ItemLoc> {
    return items.map(({img, aabb}) => ({
      kind: 'item',
      img: img,
      minX: aabb.min[0],
      minY: aabb.min[1],
      maxX: aabb.max[0],
      maxY: aabb.max[1],
      minZ: aabb.min[2],
    }))
  }

  _readServerBinDatas(binItemsMap: ServerBinItemMap, expiresAt: number): Array<BinData> {
    return Object.entries(binItemsMap).map(([id, items]) => ({
      id,
      expiresAt,
      items: this._readServerItems(items),
    }))
  }

  async claimLoot(session: string, path: string): Promise<ClaimResult> {
    const resp = await fetch(`${this.serverURL}claim`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        value: path,
        session: session ?? '',
      }),
    })
    const {value: {loot, hints}, session: newSession}: ServerClaim = await resp.json()
    return {
      loot: this._readServerLoot(loot),
      hints,
      session: newSession,
    }
  }

  async placeItem(session: string, itemLoc: ItemLoc): Promise<PlaceResult> {
    const resp = await fetch(`${this.serverURL}place`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        value: {
          img: itemLoc.img,
          aabb: {
            min: [itemLoc.minX, itemLoc.minY, itemLoc.minZ],
            max: [itemLoc.maxX, itemLoc.maxY, itemLoc.minZ],
          },
        },
        session: session ?? '',
      }),
    })
    const {value: {placed, user: {loot, hints}, bins}, session: newSession}: ServerPlace = await resp.json()
    const expiresAt = parseExpiresAt(resp.headers.get('Cache-Control'), DEFAULT_BIN_MAX_AGE)
    return {
      placed,
      loot: this._readServerLoot(loot),
      hints,
      binDatas: this._readServerBinDatas(bins, expiresAt),
      session: newSession,
    }
  }

  async fetchView(viewLoc: Loc): Promise<ViewData> {
    const params = new URLSearchParams({
      minPoint: [viewLoc.minX, viewLoc.minY].join(','),
      maxPoint: [viewLoc.maxX, viewLoc.maxY].join(','),
    })
    const resp = await fetch(`${this.serverURL}view?${params}`)
    const data: ServerBinSpace = await resp.json()
    return {
      expiresAt: parseExpiresAt(resp.headers.get('Cache-Control'), DEFAULT_VIEW_MAX_AGE),
      bins: Object.entries(data).map(([id, aabb]) => ({
        kind: 'bin',
        id,
        minX: aabb.min[0],
        minY: aabb.min[1],
        maxX: aabb.max[0],
        maxY: aabb.max[1],
      })),
    }
  }

  async fetchBin(id: string): Promise<BinData> {
    const resp = await fetch(`${this.serverURL}bin/${id}`)
    const items: ServerItems = await resp.json()
    return {
      id,
      expiresAt: parseExpiresAt(resp.headers.get('Cache-Control'), DEFAULT_BIN_MAX_AGE),
      items: this._readServerItems(items),
    }
  }
}

export class MapleAdminClient extends MapleClient {
  async fetchAllItems(username: string, password: string): Promise<Array<InventoryItem>> {
    const resp = await fetch(`${this.serverURL}mod/loot/all`, {
      headers: {
        'Authorization': `Basic ${btoa(`${username}:${password}`)}`,
      },
    })
    const data: ServerItems = await resp.json()
    return this._readServerLoot(data)
  }

  async placeAdminItem(username: string, password: string, itemLoc: ItemLoc): Promise<Array<BinData>> {
    const resp = await fetch(`${this.serverURL}mod/loot/place`, {
      method: 'POST',
      headers: {
        'Authorization': `Basic ${btoa(`${username}:${password}`)}`,
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        img: itemLoc.img,
        aabb: {
          min: [itemLoc.minX, itemLoc.minY, itemLoc.minZ],
          max: [itemLoc.maxX, itemLoc.maxY, itemLoc.minZ],
        }
      }),
    })
    const binItemsMap: ServerBinItemMap = await resp.json()
    const expiresAt = parseExpiresAt(resp.headers.get('Cache-Control'), DEFAULT_BIN_MAX_AGE)
    return this._readServerBinDatas(binItemsMap, expiresAt)
  }

  async removeItem(username: string, password: string, itemLoc: ItemLoc): Promise<Array<BinData>> {
    const resp = await fetch(`${this.serverURL}mod/remove`, {
      method: 'POST',
      headers: {
        'Authorization': `Basic ${btoa(`${username}:${password}`)}`,
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        img: itemLoc.img,
        aabb: {
          min: [itemLoc.minX, itemLoc.minY, itemLoc.minZ],
          max: [itemLoc.maxX, itemLoc.maxY, itemLoc.minZ],
        }
      }),
    })
    const binItemsMap: ServerBinItemMap = await resp.json()
    const expiresAt = parseExpiresAt(resp.headers.get('Cache-Control'), DEFAULT_BIN_MAX_AGE)
    return this._readServerBinDatas(binItemsMap, expiresAt)
  }
}
