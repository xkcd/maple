import React, {useCallback, useLayoutEffect, useRef, useState, useMemo} from 'react'
import throttle from 'lodash/throttle'

import type {ItemLoc} from '../types'
import lootImageURL from '../lootImageURL'
import useCanvas from '../useCanvas'

function draw(
  canvas: HTMLCanvasElement,
  dpr: number,
  width: number,
  height: number,
  x: number,
  y: number,
  scale: number,
  items: Iterable<ItemLoc>,
  getImage: (url: string) => HTMLImageElement,
) {
  const debugEnabled = localStorage.getItem('mapDebug') === 'true'
  const ctx = canvas.getContext('2d')
  ctx.save()
  ctx.scale(dpr, dpr)
  ctx.clearRect(0, 0, width, height)
  ctx.scale(scale, scale)
  ctx.translate(-x, y)

  const top = height / scale
  for (const item of items) {
    const img = getImage(lootImageURL(item.img))
    if (img) {
      // If an image is in "broken" error state it can cause the canvas render to crash. We filter those out in MapLoader, but should also be defensive here.
      try {
        ctx.drawImage(img, item.minX, top - item.maxY, item.maxX - item.minX, item.maxY - item.minY)
      } catch (err) {
        console.warn('Error drawing image', item, err)
      }
    }
    if (debugEnabled) {
      ctx.strokeStyle = 'red'
      ctx.fillText(`${item.minX}, ${item.minY}`, item.minX, height - item.minY)
      ctx.strokeRect(item.minX, top - item.maxY, item.maxX - item.minX, item.maxY - item.minY)
    }
  }

  if (y < 0) {
    ctx.globalCompositeOperation = 'destination-over'
    ctx.fillStyle = 'white'
    ctx.fillRect(x, top, width / scale, -y)
    ctx.globalCompositeOperation = 'difference'
    ctx.fillStyle = 'white'
    ctx.fillRect(x, top, width / scale, -y)
  }

  ctx.restore()
}

const MapRender: React.FC<{
  width: number,
  height: number,
  x: number,
  y: number,
  scale: number,
  items: Iterable<ItemLoc>,
  style?: React.CSSProperties,
  onClick?: (ev: React.MouseEvent) => void,
}> = ({width, height, x, y, scale, items, style, onClick}) => {
  const [dpr, canvasWidth, canvasHeight, canvasRef] = useCanvas(width, height)
  const [imagesUpdated, setImagesUpdated] = useState({})
  const imgCache = useRef(new Map<string, HTMLImageElement>())

  const redrawWithNewImages = useMemo(() => throttle(() => {
    setImagesUpdated({})
  }, 500), [])

  const getImage = useCallback((url: string) => {
    const map = imgCache.current
    if (map.has(url)) {
      return map.get(url)
    } else {
      map.set(url, null)
      const img = new Image()
      img.addEventListener('load', () => {
        map.set(url, img)
        // Force update.
        redrawWithNewImages()
      })
      img.src = url
    }
  }, [])

  useLayoutEffect(() => {
    draw(canvasRef.current, dpr, width, height, x, y, scale, items, getImage)
  }, [dpr, width, height, x, y, items, imagesUpdated])

  return (
    <canvas
      ref={canvasRef}
      width={canvasWidth}
      height={canvasHeight}
      style={{width, height, ...style}}
      onClick={onClick}
    />
  )
}

export default MapRender
