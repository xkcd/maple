import React, {useCallback, useMemo} from 'react'
import styled, {keyframes} from 'styled-components'

import type {ArtImage, StoredInventoryItem} from '../types'
import comicData from '../../comic'
import lootImageURL from '../lootImageURL'
import LootButton from './LootButton'
import Text, {SimpleText} from '../Text'

const xImg = require('../../art/x.png')
const upImg = require('../../art/up.png')

const LootPane: React.FC<{
  isExpanded: boolean,
  isEditingMap: boolean,
  isColliding: boolean,
  selectedItemId: null | string,
  loot: Array<StoredInventoryItem>,
  hint: null | string,
  firstNewLoot?: StoredInventoryItem,
  openNewLoot?: () => void,
  onToggleExpanded?: () => void,
  onChangeSelected: (id: null | string) => void,
  onConfirmPlacement: () => void,
}> = ({isExpanded, isEditingMap, isColliding, selectedItemId, loot, hint, firstNewLoot, openNewLoot, onToggleExpanded, onChangeSelected, onConfirmPlacement}) => {
  const handleItemSelect = useCallback((ev) => {
    onChangeSelected(ev.target.dataset.id)
  }, [onChangeSelected])

  const handleResetSelected = useCallback(() => {
    onChangeSelected(null)
  }, [onChangeSelected])

  let commandContent
  if (isEditingMap) {
    if (isColliding) {
      commandContent = <Text>Move your item to an empty space.</Text>
    } else if (selectedItemId) {
      commandContent = <>
        <Text>Confirm position?</Text>
        <CommandButton
          onClick={onConfirmPlacement}
        >
          <Text>Place it</Text>
        </CommandButton>
        <CommandButton
          onClick={handleResetSelected}
        >
          <Text>Cancel</Text>
        </CommandButton>
      </>
    } else if (loot.length) {
      commandContent = <Text>Select an item to place it.</Text>
    }
  } else {
    commandContent = <SimpleText>Place your items in the <Link href={comicData.url}>Collector's Edition</Link></SimpleText>
  }

  const lootItemContent = useMemo(() => (
    loot.map(item =>
      <LootItem
        key={item.id}
        isSelected={item.id === selectedItemId}
        data-id={item.id}
        onClick={handleItemSelect}
        style={{
          backgroundImage: `url(${lootImageURL(item.img)})`
        }}
      />
    )
  ), [loot, selectedItemId, handleItemSelect])

  return (
    <StyledLootPane isExpanded={isExpanded}>
      {onToggleExpanded && <ToggleButton
          onClick={onToggleExpanded}
        >
          <Text>Backpack</Text>
          <img
            src={(isExpanded ? xImg : upImg).url['2x']}
            width={(isExpanded ? xImg : upImg).width}
            height={(isExpanded ? xImg : upImg).height}
          />
      </ToggleButton>}
      <AboveBar isHidden={!isExpanded}>
        {commandContent && <CommandBar>{commandContent}</CommandBar>}
        <Spacer />
        {loot.length > 0 && hint && <Hint>Hint: {hint}</Hint>}
      </AboveBar>
      <LootTray>
        {lootItemContent}
        {loot.length === 0 && hint && <FillHint>Hint: {hint}</FillHint>}
      </LootTray>
      {openNewLoot && <PositionedLootButton
          firstNewLoot={firstNewLoot}
          openNewLoot={openNewLoot}
      />}
    </StyledLootPane>
  )
}

const expandDuration = .5

interface AboveBarProps {
  isHidden: boolean,
}
const AboveBar = styled.div<AboveBarProps>`
  display: flex;
  align-items: flex-end;
  flex-wrap: wrap;
  position: absolute;
  bottom: 157px;
  left: 0;
  right: 200px;
  padding: 0 10px;
  opacity: ${({isHidden}) => isHidden ? 0 : 1};
  transition: opacity ${expandDuration}s ease;

  & > * {
    margin-bottom: 5px;
  }
`

const Spacer = styled.div`
  flex: 1;
  min-width: 50px;
`

const Hint = styled(Text)`
  flex-shrink: 0;
  background: white;
  border: 2px solid black;
  border-radius: 4px;
  padding: 5px 10px;
`

const CommandButton = styled.button`
  box-sizing: content-box;
  background: #ccc;
  border: none;
  border-radius: 2px;
  padding: 0 5px;
  font-size: 22px;
`

const CommandBar = styled.div`
  display: flex;
  flex-shrink: 0;
  background: white;
  border: 2px solid black;
  padding: 5px 10px;
  font-size: 22px;

  ${CommandButton} {
    margin-left: 15px;
  }
`

const FillHint = styled(Text)`
  align-self: center;
  text-align: center;
  flex: 1;
  color: black;
`

const Link = styled.a`
  color: blue;
  font-weight: normal;
  text-decoration: underline;
`

interface LootItemProps {
  isSelected: boolean,
}
const LootItem = styled.div<LootItemProps>`
  width: 100px;
  height: 100px;
  box-sizing: border-box;
  flex-shrink: 0;
  background: center center no-repeat;
  background-color: ${({isSelected}) => isSelected ? '#ddd' : '#fff'};
  background-size: contain;
  border-color: ${({isSelected}) => isSelected ? '#ddd' : '#fff'};
  border-style: solid;
  border-width: 10px;
  border-radius: 2px;

  &:hover {
    background-color: #eee;
    border-color: #eee;
  }
`

const LootTray = styled.div`
  display: flex;
  height: 140px;
  padding: 5px;
  background: white;
  flex-wrap: wrap;
  overflow-y: auto;
`

const PositionedLootButton = styled(LootButton)`
  position: absolute;
  top: -230px;
  right: 0;
`

const ToggleButton = styled.div`
  position: absolute;
  display: flex;
  align-items: center;
  top: -40px;
  right: 34px;
  height: 30px;
  font-size: 22px;
  padding: 4px 10px;
  background: white;
  border-top-left-radius: 4px;
  border-top-right-radius: 4px;
  border: 2px solid black;
  border-bottom-width: 0;
  user-select: none;
  cursor: pointer;

  & > * {
    margin: 0 6px;
  }
`

interface StyledLootPaneProps {
  isExpanded: boolean,
}
const StyledLootPane = styled.div<StyledLootPaneProps>`
  position: fixed;
  left: 0;
  bottom: 0;
  width: 100%;
  background: white center center;
  border-top: 2px solid black;
  transform: ${({isExpanded}) => isExpanded ? 'none' : 'translate(0, 100%)'};
  transition: transform ${expandDuration}s ease;
`

export default LootPane
