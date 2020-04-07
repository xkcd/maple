import RBush, {BBox} from 'rbush'

import type {ArtImage} from './types'
import type {MapleComicGlobal} from './index'

// Extend rbush types to expose internal bounding box of items
declare module 'rbush' {
   export default interface RBush<T> {
      data: BBox,
   }
}

declare module "*.png" {
  const content: ArtImage
  export default content
}

declare global {
  interface Window { MapleComic: MapleComicGlobal; }
}
