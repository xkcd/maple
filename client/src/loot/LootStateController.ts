import countBy from 'lodash/countBy'
import find from 'lodash/find'
import findIndex from 'lodash/findIndex'
import sample from 'lodash/sample'
import {Emitter} from '@servie/events'
import {v4 as uuidv4} from 'uuid'

import type {ZLoc, ItemLoc, InventoryItem, StoredInventoryItem, InventoryState, BinData} from '../types'
import type {APIClient} from '../APIClient'

const SESSION_KEY = 'session'
const INVENTORY_KEY = 'inventory'
const SEEN_KEY = 'seen'

interface LootEvents {
  change: [InventoryState],
}

export default class LootStateController {
  api: APIClient
  lastHint: string
  events: Emitter<LootEvents>

  constructor(api: APIClient) {
    this.api = api
    this.events = new Emitter<LootEvents>()
    this.lastHint = null
    window.addEventListener('storage', this._handleStorageChange)
  }

  get sessionData(): null | string {
    return localStorage.getItem(SESSION_KEY)
  }

  get seenIds(): Set<string> {
    return new Set(JSON.parse(localStorage.getItem(SEEN_KEY) || '[]'))
  }

  get inventory(): Array<StoredInventoryItem> {
    return JSON.parse(localStorage.getItem(INVENTORY_KEY) || '[]')
  }

  get state(): InventoryState {
    const {seenIds, inventory} = this
    const firstNewLoot = inventory.find(i => !seenIds.has(i.id))
    const seenInventory = inventory.filter(i => seenIds.has(i.id))
    return {
      firstNewLoot,
      loot: seenInventory,
      hint: this.lastHint,
    }
  }

  _emitState() {
    this.events.emit('change', this.state)
  }

  _handleStorageChange = (ev: StorageEvent) => {
    if (ev.key === INVENTORY_KEY || ev.key === SEEN_KEY) {
      this._emitState()
    }
  }

  _handleServerData({loot, hints, session}: {loot: Array<InventoryItem>, hints: Array<string>, session: string}) {
    localStorage.setItem(SESSION_KEY, session)
    this._storeServerLoot(loot)
    this.lastHint = sample(hints)
    this._emitState()
  }

  _storeServerLoot(serverInventory: Array<InventoryItem>) {
    const newInventory = [...this.inventory]

    const serverCounts = countBy(serverInventory, i => i.img)
    const localCounts = countBy(this.inventory, i => i.img)
    const imgKeys = new Set([...Object.keys(serverCounts), ...Object.keys(localCounts)])
    for (const img of imgKeys) {
      const serverCount = serverCounts[img] || 0
      const localCount = localCounts[img] || 0

      if (serverCount > localCount) {
        for (let i = 0; i < serverCount - localCount; i++) {
          newInventory.unshift({
            id: uuidv4(),
            ...find(serverInventory, i => i.img === img),
          })
        }
      } else if (serverCount < localCount) {
        for (let i = 0; i < localCount - serverCount; i++) {
          const removeIdx = findIndex(newInventory, i => i.img === img)
          newInventory.splice(removeIdx, 1)
        }
      }
    }

    localStorage.setItem(INVENTORY_KEY, JSON.stringify(newInventory))
  }

  async claimLoot(path: string) {
    const {sessionData} = this
    const {loot, hints, session} = await this.api.claimLoot(sessionData, path)
    this._handleServerData({loot, hints, session})
  }

  openNewLoot() {
    const seenIds = this.seenIds
    const firstNewLoot = this.inventory.find(i => !seenIds.has(i.id))
    if (firstNewLoot) {
      const newSeenIds = [firstNewLoot.id, ...seenIds]
      localStorage.setItem(SEEN_KEY, JSON.stringify(newSeenIds))
      this._emitState()
    }
  }

  async placeItem(id: string, itemLoc: ItemLoc): Promise<false | Array<BinData>> {
    const {inventory, seenIds, sessionData} = this

    const {placed, loot, hints, binDatas, session} = await this.api.placeItem(sessionData, itemLoc)

    if (placed) {
      // Remove the specific item from our local stores first.
      const newSeenIds = [...seenIds].filter(sid => sid !== id)
      const newInventory = inventory.filter(i => i.id !== id)
      localStorage.setItem(INVENTORY_KEY, JSON.stringify(newInventory))
      localStorage.setItem(SEEN_KEY, JSON.stringify(newSeenIds))
    }

    // Make sure everything matches up with the server's loot data.
    this._handleServerData({loot, hints, session})

    return placed && binDatas
  }
}
