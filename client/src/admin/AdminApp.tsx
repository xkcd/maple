import React, {useCallback, useEffect, useState} from 'react'
import styled from 'styled-components'
import {useAsyncCallback} from 'react-async-hook'

import type {MapleAdminClient} from '../APIClient'
import type MapLoader from '../comic/MapLoader'
import type {ItemPlaceState, ItemLoc} from '../types'
import lootImageURL from '../lootImageURL'
import useStorage from '../useStorage'
import Comic from '../comic/Comic'
import LootPane from '../loot/LootPane'
import Text from '../Text'
import Input from './Input'

const AdminApp: React.FC<{
  api: MapleAdminClient,
  mapLoader: MapLoader,
}> = ({api, mapLoader}) => {
  const [username, setUsername] = useStorage('adminUsername', '')
  const [password, setPassword] = useStorage('adminPassword', '')
  const [scale, setScale] = useState<number>(1)
  const [selectedItemId, setSelectedItemId] = useState<string>(null)
  const [itemPlaceState, setItemPlaceState] = useState<ItemPlaceState>(null)
  const [clickedItems, setClickedItems] = useState<Array<ItemLoc>>([])

  const asyncFetchAllItems = useAsyncCallback(async () => {
    const items = await api.fetchAllItems(username, password)
    return items.map(i => ({id: i.img, ...i}))
  })
  const allItems = asyncFetchAllItems.result
  const itemToPlace = selectedItemId !== null && scale === 1 && allItems && allItems.find(i => i.id === selectedItemId)

  const asyncPlaceItem = useAsyncCallback(async (itemLoc: ItemLoc) => {
    const updateBinDatas = await api.placeAdminItem(username, password, itemLoc)
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

  const asyncRemoveItem = useAsyncCallback(async (itemLoc: ItemLoc) => {
    const updateBinDatas = await api.removeItem(username, password, itemLoc)
    mapLoader.updateBins(updateBinDatas)
    setClickedItems((clickedItems) => clickedItems.filter(i => i !== itemLoc))
  })

  const scale1x = useCallback(() => {setScale(1)}, [])
  const scaleHalf = useCallback(() => {setScale(.5)}, [])
  const scaleThird = useCallback(() => {setScale(.3)}, [])
  const scaleQuarter = useCallback(() => {setScale(.25)}, [])

  let selectionContent
  if (clickedItems.length) {
    selectionContent = (
      <>
        <Text>Clicked items:</Text>
        {clickedItems.map(itemLoc =>
          <ClickedItem key={itemLoc.img}>
            <LootIcon style={{
              backgroundImage: `url(${lootImageURL(itemLoc.img)})`
            }} />
            <Text>{itemLoc.img}: {itemLoc.minX}, {itemLoc.minY}, {itemLoc.minZ}</Text>
            <StyledButton
              onClick={() => {asyncRemoveItem.execute(itemLoc)}}
            >
              Delete
            </StyledButton>
          </ClickedItem>
        )}
      </>
    )
  } else {
    selectionContent = <Text>Click the map to list items.</Text>
  }

  return (
    <>
      <Main>
        <StyledComic
          mapLoader={mapLoader}
          altText="You're a wizard, Harry!"
          width={1200}
          height={800}
          scale={scale}
          itemToPlace={itemToPlace}
          onItemPlaceStateChange={setItemPlaceState}
          onClickItems={setClickedItems}
        />
        <AdminSidebar>
          <Text>Log in:</Text>
          <StyledInput
            type="text"
            placeholder="username"
            value={username}
            onChangeValue={setUsername}
          />
          <StyledInput
            type="password"
            placeholder="password"
            value={password}
            onChangeValue={setPassword}
          />
          <StyledButton
            onClick={asyncFetchAllItems.execute}
          >
            Login
          </StyledButton>
          <Separator />
          <ZoomButtons>
            <StyledButton onClick={scale1x}>1x</StyledButton>
            <StyledButton onClick={scaleHalf}>.5x</StyledButton>
            <StyledButton onClick={scaleThird}>.3x</StyledButton>
            <StyledButton onClick={scaleQuarter}>.25x</StyledButton>
          </ZoomButtons>
          <Separator />
          {selectionContent}
        </AdminSidebar>
      </Main>
      <LootPane
        isExpanded={true}
        isEditingMap={true}
        isColliding={false}  // Admins can bend the laws of space.
        selectedItemId={selectedItemId}
        loot={allItems || []}
        hint="With great power comes..."
        onChangeSelected={setSelectedItemId}
        onConfirmPlacement={handleConfirmPlacement}
      />
    </>
  )
}

const Main = styled.div`
  display: flex;
`

const StyledComic = styled(Comic) `
  flex-shrink: 0;
`

const AdminSidebar = styled.div`
  display: flex;
  flex-direction: column;
  flex: 1;
  margin-left: 20px;

  &, & input, & button {
    font-size: 20px;
  }

  & input, & button {
    width: 10em;
  }
`

const StyledInput = styled(Input)`
  margin: 5px 0;
  padding: 5px;
  font-family: xkcd-Regular-v3;
  border: 2px solid black;
`

const StyledButton = styled.button`
  box-sizing: content-box;
  margin: 5px 0;
  padding: 5px 10px;
  font-family: xkcd-Regular-v3;
  border: 2px solid black;
`

const Separator = styled.div`
  margin-top: 10px;
  padding-bottom: 10px;
  border-top: 2px solid black
`

const ClickedItem = styled.div `
  margin: 10px 0;

  & > * {
    margin-right: 20px;
  }

  & > button {
    width: auto;
  }
`

const LootIcon = styled.div`
  width: 50px;
  height: 50px;
  padding: 5px;
  background: white center center no-repeat;
  background-size: contain;
`

const ZoomButtons = styled.div`
  display: flex;

  & > button {
    margin-right: 10px;
    width: 5em;
  }
`

export default AdminApp
