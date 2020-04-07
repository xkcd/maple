import {useRef, useLayoutEffect, useState} from 'react'

import type {ZItemMap, ZLoc, ItemLoc} from '../types'
import type {APIClient} from '../APIClient'
import type MapLoader from './MapLoader'
import type {RenderMetrics} from './MapLoader'

interface MapLoaderMethods {
  setView: (x: number, y: number, width: number, height: number, scale: number) => void,
  checkCollision: (zloc: ZLoc) => boolean,
  getItemsAt: (zloc: ZLoc) => Array<ItemLoc>,
}

export default function useMapLoader(mapLoader: MapLoader): [Array<ItemLoc>, RenderMetrics, MapLoaderMethods] {
  const [metrics, setMetrics] = useState(null)
  const [items, setItems] = useState([])

  useLayoutEffect(() => {
    mapLoader.events.on('metrics', setMetrics)
    mapLoader.events.on('update', setItems)
    return () => {
      mapLoader.events.off('metrics', setMetrics)
      mapLoader.events.off('update', setItems)
    }
  }, [])

  const methods = {
    setView: mapLoader.view,
    checkCollision: mapLoader.checkCollision,
    getItemsAt: mapLoader.getItemsAt,
  }

  return [items, metrics, methods]
}
