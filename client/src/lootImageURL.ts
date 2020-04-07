import comicData from '../comic'

export default function lootImageURL(imgName: string) {
  return comicData.itemImgBaseURL + imgName
}
