import React, {useCallback, useEffect, useState} from 'react'
import ReactDOM from 'react-dom'
import {useAsyncCallback} from 'react-async-hook'

import type {APIClient} from './APIClient'
import type {ItemPlaceState, ItemLoc} from './types'
import type MapLoader from './comic/MapLoader'
import type LootStateController from './loot/LootStateController'
import comicData from '../comic'
import useStorage from './useStorage'
import useLootState from './loot/useLootState'
import Comic from './comic/Comic'
import LootPane from './loot/LootPane'

const App: React.FC<{
  api: APIClient,
  mapLoader: MapLoader,
  lootStateController: LootStateController,
  comicEl: null | Element,
}> = ({api, mapLoader, lootStateController, comicEl}) => {
  const [lootState, openNewLoot, placeItem] = useLootState(api, lootStateController)
  const [selectedItemId, setSelectedItemId] = useState<string>(null)
  const [itemPlaceState, setItemPlaceState] = useState<ItemPlaceState>(null)
  const [paneExpanded, setPaneExpanded] = useStorage('inventoryExpanded', true)

  const itemToPlace = selectedItemId !== null && lootState.loot.find(i => i.id === selectedItemId)

  const handleToggleExpanded = useCallback(() => {
    setPaneExpanded(!paneExpanded)
  }, [paneExpanded])

  const asyncPlaceItem = useAsyncCallback(async (itemLoc: ItemLoc) => {
    const updateBinDatas = await placeItem(selectedItemId, itemLoc)
    if (!updateBinDatas) {
      // Placement failed.
      return
    }
    mapLoader.updateBins(updateBinDatas)
    setSelectedItemId(null)
  })

  const handleConfirmPlacement = useCallback(() => {
    if (!itemPlaceState) {
      throw new Error('Missing item place state')
    }
    const itemLoc: ItemLoc = {
      kind: 'item',
      img: itemToPlace.img,
      ...itemPlaceState.zloc,
    }
    asyncPlaceItem.execute(itemLoc)
  }, [itemPlaceState])

  let comicPortal
  if (comicEl) {
    const comic = (
      <Comic
        mapLoader={mapLoader}
        altText={comicData.alt}
        width={comicData.width}
        height={comicData.height}
        itemToPlace={itemToPlace}
        onItemPlaceStateChange={setItemPlaceState}
        showMovementTip
      />
    )
    comicPortal = ReactDOM.createPortal(comic, comicEl)
  }

  return (
    <>
      {comicPortal}
      <LootPane
        isExpanded={paneExpanded}
        isEditingMap={!!comicPortal}
        isColliding={itemPlaceState?.isColliding}
        selectedItemId={selectedItemId}
        loot={lootState.loot}
        hint={lootState.hint}
        firstNewLoot={lootState.firstNewLoot}
        onToggleExpanded={handleToggleExpanded}
        onChangeSelected={setSelectedItemId}
        onConfirmPlacement={handleConfirmPlacement}
        openNewLoot={openNewLoot}
      />
    </>
  )
}

export default App
