import React, {useRef} from 'react'

export default function useCanvas(width: number, height: number): [number, number, number, React.RefObject<HTMLCanvasElement>] {
  const dpr = window.devicePixelRatio || 1
  const canvasWidth = Math.round(width * dpr)
  const canvasHeight = Math.round(height * dpr)
  const canvasRef = useRef()
  return [dpr, canvasWidth, canvasHeight, canvasRef]
}
