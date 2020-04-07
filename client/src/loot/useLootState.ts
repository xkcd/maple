import React, {useCallback, useLayoutEffect, useRef, useState} from 'react'

import type {ItemLoc, StoredInventoryItem, InventoryState, BinData} from '../types'
import type {APIClient} from '../APIClient'
import LootStateController from './LootStateController'

export default function useLootState(api: APIClient, lootStateController: LootStateController): [InventoryState, () => void, (id: string, itemLoc: ItemLoc) => Promise<false | Array<BinData>>] {
  const [lootState, setLootState] = useState<InventoryState>(lootStateController.state)

  useLayoutEffect(() => {
    lootStateController.events.on('change', setLootState)
    return () => {
      lootStateController.events.off('change', setLootState)
    }
  }, [])

  const openNewLoot = useCallback(() => lootStateController.openNewLoot(), [])
  const placeItem = useCallback((id: string, itemLoc: ItemLoc) => lootStateController.placeItem(id, itemLoc), [])
  return [lootState, openNewLoot, placeItem]
}
