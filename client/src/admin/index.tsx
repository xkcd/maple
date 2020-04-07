import React from 'react'
import ReactDOM from 'react-dom'

import comicData from '../../comic'
import {MapleAdminClient} from '../APIClient'
import AdminApp from './AdminApp'
import MapLoader from '../comic/MapLoader'

function main() {
  const api = new MapleAdminClient(comicData.apiServerURL)
  const mapLoader = new MapLoader(api, {renderExtraScreens: 0})
  const app = (
    <AdminApp
      api={api}
      mapLoader={mapLoader}
    />
  )
  ReactDOM.render(app, document.body)
}

document.addEventListener('DOMContentLoaded', main)
