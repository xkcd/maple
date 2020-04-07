import type {ArtImage} from './types'

export interface PreloadedArtImage extends ArtImage {
  el: HTMLImageElement,
}

export function preloadImg(imgData: ArtImage): PreloadedArtImage {
  const el = new Image()
  setTimeout(() => {
    el.src = imgData.url['2x']
  }, 0)
  return {el, ...imgData}
}
