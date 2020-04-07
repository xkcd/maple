import isInteger from 'lodash/isInteger'
import debounce from 'lodash/debounce'
import React, {useCallback, useEffect, useLayoutEffect, useRef, useState, useMemo} from 'react'
import styled from 'styled-components'

import type MapLoader from './MapLoader'
import type {Loc, ZLoc, StoredInventoryItem, ItemPlaceState, ItemLoc} from '../types'
import type {DragSettings} from './useDragPos'
import lootImageURL from '../lootImageURL'
import Text from '../Text'
import MapRender from './MapRender'
import useDragPos from './useDragPos'
import useMapLoader from './useMapLoader'

const BORDER_WIDTH = 2

function locProps(loc?: Loc) {
  return [
    loc?.minX,
    loc?.minY,
    loc?.maxX,
    loc?.maxY,
  ]
}

function zlocProps(zloc?: ZLoc) {
  return [...locProps(zloc), zloc?.minZ]
}

function parseHash(hash: string): {x: number, y: number} {
  const match = hash.substr(1).match(/(-?\d+),(-?\d+)/)
  if (!match) {
    return
  }
  const x = -Number(match[1])
  const y = -Number(match[2])
  if (!isInteger(x) || !isInteger(y)) {
    return
  }
  return {x, y}
}

function useHashSync(panX: number, panY: number, resetPan: (settings: DragSettings) => void) {
  const handleHashChange = useCallback(() => {
    const pos = parseHash(location.hash)
    if (pos) {
      resetPan({pos})
    }
  }, [])

  const handlePanChange = useMemo(() => debounce((panX: number, panY: number) => {
    location.hash = `${-panX},${-panY}`
  }, 500), [])

  useEffect(() => {
    window.addEventListener('hashchange', handleHashChange)
    return () => {
      window.removeEventListener('hashchange', handleHashChange)
    }
  })

  useEffect(() => {
    handlePanChange(panX, panY)
  }, [panX, panY])

  return parseHash(location.hash)
}

const Comic: React.FC<{
  className?: string,
  mapLoader: MapLoader,
  altText: string,
  width: number,
  height: number,
  scale?: number,
  itemToPlace: StoredInventoryItem,
  showMovementTip?: boolean,
  onItemPlaceStateChange: (state: ItemPlaceState) => void,
  onClickItems?: (items: Array<ItemLoc>) => void,
}> = ({className, mapLoader, altText, width, height, scale = 1, itemToPlace, showMovementTip, onItemPlaceStateChange, onClickItems}) => {
  const clipElRef = useRef()
  const [panState, panEventHandlers, resetPan] = useDragPos({
    pos: parseHash(location.hash) || {
      x: Math.floor(width / (2 * scale)),
      y: Math.floor(height / (2 * scale)),
    },
  })
  useHashSync(panState.x, panState.y, resetPan)
  const [placeState, placeEventHandlers, resetPlace] = useDragPos()
  const [itemIsColliding, setItemIsColliding] = useState(false)
  const [items, metrics, {setView, checkCollision, getItemsAt}] = useMapLoader(mapLoader)

  const innerWidth = width - 2 * BORDER_WIDTH
  const innerHeight = height - 2 * BORDER_WIDTH

  useLayoutEffect(() => {
    setView(-panState.x, -panState.y, innerWidth, innerHeight, scale)
  }, [panState.x, panState.y, scale])

  useLayoutEffect(() => {
    if (!metrics) {
      return
    }
    // Drag coordinate system is inverted from map, so we have to flip the bounds.
    resetPan({
      bounds: {
        minX: -metrics.viewBounds.maxX,
        minY: -metrics.viewBounds.maxY,
        maxX: -metrics.viewBounds.minX,
        maxY: -metrics.viewBounds.minY,
      },
    })
  }, locProps(metrics?.viewBounds))

  useLayoutEffect(() => {
    if (!itemToPlace) {
      return
    }
    const halfItemWidth = Math.floor(itemToPlace.width / 2)
    const halfItemHeight = Math.floor(itemToPlace.height / 2)
    resetPlace({
      pos: {
        x: Math.floor(innerWidth / 2 - halfItemWidth),
        y: Math.floor(innerHeight / 2 - halfItemHeight),
      },
      bounds: {
        minX: -halfItemWidth,
        maxX: innerWidth - halfItemWidth,
        minY: -halfItemHeight,
        maxY: innerHeight - halfItemHeight,
      },
    })
  }, [itemToPlace?.id])

  const itemToPlaceLoc = itemToPlace && {
    minX: placeState.x - panState.x,
    maxX: placeState.x + itemToPlace.width - panState.x,
    minY: placeState.y - panState.y,
    maxY: placeState.y + itemToPlace.height - panState.y,
    minZ: itemToPlace.z,
  }

  useEffect(() => {
    if (!itemToPlace) {
      return
    }
    const isColliding = checkCollision(itemToPlaceLoc)
    setItemIsColliding(isColliding)
    onItemPlaceStateChange({
      zloc: itemToPlaceLoc,
      isColliding,
    })
  }, [items, ...zlocProps(itemToPlaceLoc)])

  const handleClickItems = useCallback((ev) => {
    if (!metrics) {
      return
    }
    const x = metrics.minX + ev.nativeEvent.offsetX
    const y = metrics.minY + (metrics.height - ev.nativeEvent.offsetY)
    const zloc = {
      minX: x,
      maxX: x,
      minY: y,
      maxY: y,
      minZ: itemToPlace.z,
    }
    onClickItems(getItemsAt(zloc))
  }, [metrics?.minX, metrics?.minY])

  return (
    <ComicOutline
      className={className}
      title={altText}
      width={width}
      height={height}
    >
      {metrics && 
        <MapClip
          ref={clipElRef}
          width={innerWidth}
          height={innerHeight}
          {...panEventHandlers}
        >
          <MapRender
            width={metrics.width}
            height={metrics.height}
            x={metrics.minX}
            y={metrics.minY}
            scale={scale}
            items={items}
            style={{transform: `translate(${metrics.offsetX}px, ${metrics.offsetY}px)`}}
            onClick={onClickItems ? handleClickItems : null}
          />
        </MapClip>
      }
      {!metrics && <LoadingText>[Loading...]</LoadingText>}
      {itemToPlace && <LootPlacer
        key={itemToPlace.id}
        isColliding={itemIsColliding}
        style={{
          transform: `translate(${placeState.x}px, ${innerHeight - placeState.y - itemToPlace.height}px)`,
        }}
        {...placeEventHandlers}
      >
        <MapRender
          width={itemToPlace.width * scale}
          height={itemToPlace.height * scale}
          x={itemToPlaceLoc.minX}
          y={itemToPlaceLoc.minY}
          scale={scale}
          items={[{
            kind: 'item',
            img: itemToPlace.img,
            ...itemToPlaceLoc,
          }]}
          onClick={onClickItems ? handleClickItems : null}
        />
      </LootPlacer>}
      <MovementTip isShowing={showMovementTip && !panState.hasDragged}>It's a big world out there! Drag to move around.</MovementTip>
    </ComicOutline>
  )
}

interface LootPlacerProps {
  isColliding: boolean,
}
const LootPlacer = styled.div<LootPlacerProps>`
  position: absolute;
  left: 0;
  top: 0;
  display: flex;
  outline: 2px ${({isColliding}) => isColliding ? 'solid red' : 'dotted gray'};
  outline-offset: 3px;
`

interface MapClipProps {
  width: number,
  height: number,
}
const MapClip = styled.div<MapClipProps>`
  width: ${({width}) => width}px;
  height: ${({height}) => height}px;
  overflow: hidden;
`

const LoadingText = styled(Text)`
  font-size: 24px;
  color: gray;
`

interface MovementTipProps {
  isShowing: boolean,
}
const MovementTip = styled(Text)<MovementTipProps>`
  position: absolute;
  bottom: 10px;
  font-size: 18px;
  background: white;
  border: 2px solid black;
  padding: 5px 10px;
  border-radius: 6px;
  opacity: ${({isShowing}) => isShowing ? 1 : 0};
  transition: opacity .5s ease;
  pointer-events: none;
`

interface ComicOutlineProps {
  width: number,
  height: number,
}
const ComicOutline = styled.div<ComicOutlineProps>`
  display: inline-flex;
  align-items: center;
  justify-content: center;
  position: relative;
  box-sizing: border-box;
  background: white;
  border: ${BORDER_WIDTH}px solid black;
  width: ${({width}) => width}px;
  height: ${({height}) => height}px;
  user-select: none;
  overflow: hidden;
  touch-action: none;
`

export default Comic
