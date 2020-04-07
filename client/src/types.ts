export interface ArtImage {
  width: number,
  height: number,
  url: {
    '2x': string,
  },
}

export type ServerAABB = {
  min: [number, number, number],
  max: [number, number, number],
}

export type ServerItem = {
  img: string,
  aabb: ServerAABB,
}

export type ServerItems = Array<ServerItem>

export type ServerBinSpace = {
  [key: string]: ServerAABB,
}

export type ServerBinItemMap = {
  [key: string]: ServerItems,
}

export type ServerClaim = {
  value: {
    loot: Array<ServerItem>,
    hints: Array<string>,
  },
  session: string,
}

export type ServerPlace = {
  value: {
    placed: boolean,
    user: {
      loot: Array<ServerItem>,
      hints: Array<string>,
    },
    bins: ServerBinItemMap,
  },
  session: string,
}

export type ClaimResult = {
  loot: Array<InventoryItem>,
  hints: Array<string>,
  session: string,
}

export type PlaceResult = {
  placed: boolean,
  loot: Array<InventoryItem>,
  hints: Array<string>,
  binDatas: Array<BinData>,
  session: string,
}

export interface InventoryItem {
  img: string,
  width: number,
  height: number,
  z: number,
}

export interface StoredInventoryItem extends InventoryItem {
  id: string,
}

export type InventoryState = {
  firstNewLoot: null | StoredInventoryItem,
  loot: Array<StoredInventoryItem>,
  hint: null | string,
}

export interface Loc {
  minX: number,
  maxX: number,
  minY: number,
  maxY: number,
}

export interface ZLoc extends Loc {
  minZ: number
}

export interface ItemLoc extends ZLoc {
  kind: 'item',
  img: string,
}

export type ZItemMap = Map<number, Array<ItemLoc>>

export interface BinLoc extends Loc {
  kind: 'bin',
  id: string,
}

export interface BinData {
  id: string,
  expiresAt: number,
  items: Array<ItemLoc>,
}

export interface ViewData {
  expiresAt: number,
  bins: Array<BinLoc>,
}

export interface ItemPlaceState {
  isColliding: boolean,
  zloc: ZLoc,
}
