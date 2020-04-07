import React from 'react'
import ReactDOM from 'react-dom'

import comicData from '../comic'
import {MapleClient, MockAPIClient} from './APIClient'
import App from './App'
import MapLoader from './comic/MapLoader'
import LootStateController from './loot/LootStateController'

export class MapleComicGlobal {
  comicEl: Element = null

  claimLoot: (path: string) => void

  draw(comicEl: Element) {
    comicEl.innerHTML = ''
    this.comicEl = comicEl
  }
}

function initAPI() {
  if (process.env.USE_MOCK_API) {
    return new MockAPIClient()
  } else {
    return new MapleClient(comicData.apiServerURL)
  }
}

function claimPageview(lootStateController: LootStateController) {
  const pathWithStrippedSlashes = location.pathname.replace(/^\/+|\/+$/g, '')
  lootStateController.claimLoot(pathWithStrippedSlashes)
}

function main() {
  const api = initAPI()
  const mapLoader = new MapLoader(api)
  const lootStateController = new LootStateController(api)

  if (process.env.NODE_ENV === 'development') {
    // Make it easier to claim loots on the dev console.
    window.MapleComic.claimLoot = (path: string) => {
      lootStateController.claimLoot(path)
    }
  }

  const containerEl = document.createElement('div')
  document.body.appendChild(containerEl)

  const {comicEl} = window.MapleComic

  const app = (
    <App
      api={api}
      mapLoader={mapLoader}
      lootStateController={lootStateController}
      comicEl={comicEl}
    />
  )
  ReactDOM.render(app, containerEl, () => {
    claimPageview(lootStateController)
  })
}

document.addEventListener('DOMContentLoaded', main)

window.MapleComic = new MapleComicGlobal()
