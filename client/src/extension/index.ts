declare const chrome: {
  runtime: {
    getURL: (path: string) => string
  }
}

__webpack_public_path__ = chrome.runtime.getURL('/')
