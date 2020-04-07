import {useCallback, useRef, useState} from 'react'
import clamp from 'lodash/clamp'

import {Loc} from '../types'

interface DragEvents {
  onMouseDown: (ev: any) => void,
}

interface Pos {
  x: number,
  y: number,
}

export interface DragSettings {
  pos?: Pos,
  bounds?: Loc,
}

interface DragState extends Pos {
  isDragging: boolean,
  hasDragged: boolean,
  startPos: Pos,
}

interface DragInternalState {
  bounds: null | Loc,
  startMousePos: null | Pos,
}

export default function useDragPos(settings: DragSettings = {}): [DragState, DragEvents, (settings: DragSettings) => void] {
  const internalStateRef = useRef<DragInternalState>({
    bounds: settings.bounds,
    startMousePos: null,
  })

  const [dragState, setDragState] = useState<DragState>({
    isDragging: false,
    hasDragged: false,
    x: settings.pos?.x ?? 0,
    y: settings.pos?.y ?? 0,
    startPos: null,
  })

  const handleMove = useCallback((ev) => {
    const {clientX, clientY} = ev.type === 'mousemove' ? ev : ev.touches[0]
    const {bounds, startMousePos} = internalStateRef.current
    setDragState(({x, y, startPos, ...rest}) => {
      let newX = startPos.x + (Math.round(clientX) - startMousePos.x)
      let newY = startPos.y - (Math.round(clientY) - startMousePos.y)
      if (bounds) {
        newX = clamp(newX, bounds.minX, bounds.maxX)
        newY = clamp(newY, bounds.minY, bounds.maxY)
      }
      return {
        ...rest,
        isDragging: true,
        hasDragged: true,
        x: newX,
        y: newY,
        startPos,
      }
    })
  }, [])

  const handleStop = useCallback((ev) => {
    if (ev.type === 'mouseup' && ev.button !== 0) {
      return
    } else if (ev.type === 'touchend' && ev.touches.length > 0) {
      return
    }
    window.removeEventListener('mouseup', handleStop)
    window.removeEventListener('mousemove', handleMove)
    window.removeEventListener('touchend', handleStop)
    window.removeEventListener('touchmove', handleMove)
    document.body.style.cursor = null
    internalStateRef.current.startMousePos = null
    setDragState((prevState) => ({
      ...prevState,
      isDragging: false,
      startPos: null,
    }))
  }, [])

  const handleStart = useCallback((ev) => {
    if (ev.type === 'mousedown' && ev.button !== 0) {
      return
    }
    window.addEventListener('mouseup', handleStop)
    window.addEventListener('mousemove', handleMove)
    window.addEventListener('touchend', handleStop)
    window.addEventListener('touchmove', handleMove)
    document.body.style.cursor = 'grabbing'
    const {clientX, clientY} = ev.type === 'mousedown' ? ev : ev.touches[0]
    internalStateRef.current.startMousePos = {
      x: Math.round(clientX),
      y: Math.round(clientY),
    }
    setDragState((prevState) => ({
      ...prevState,
      isDragging: true,
      startPos: {
        x: prevState.x,
        y: prevState.y,
      },
    }))
  }, [])

  const reset = useCallback((settings: DragSettings) => {
    setDragState((prevState) => ({
      ...prevState,
      x: settings.pos?.x ?? prevState.x,
      y: settings.pos?.y ?? prevState.y,
    }))
    internalStateRef.current = {
      ...internalStateRef.current,
      bounds: settings.bounds,
    }
  }, [])

  // TODO touch
  const dragEventHandlers = {
    onMouseDown: handleStart,
    onTouchStart: handleStart,
  }

  return [dragState, dragEventHandlers, reset]
}
