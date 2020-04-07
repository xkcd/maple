const {callbackify} = require('util')
const loaderUtils = require('loader-utils')
const sharp = require('sharp')

async function processImage(inputBuffer) {
  const options = loaderUtils.getOptions(this)

  const img = sharp(inputBuffer)
  const meta = await img.metadata()

  const {
    data: resized2XBuffer,
    info: resized2XInfo,
  } = await img.resize({
    width: Math.floor(meta.width * options.scale) * 2,
  })
    .png({palette: !!options.quant})
    .toBuffer({resolveWithObject: true})

  const interpolatedName = loaderUtils.interpolateName(
    this,
    options.name,
    {content: resized2XBuffer},
  )
  this.emitFile(interpolatedName, resized2XBuffer)

  const publicPath = options.publicPath ? options.publicPath + '/' : ''
  return `module.exports = {
    width: ${resized2XInfo.width / 2},
    height: ${Math.floor(resized2XInfo.height / 2)},
    url: {
      '2x': __webpack_public_path__ + '${interpolatedName}',
    }
  }`
}

const processImageCb = callbackify(processImage)

module.exports = function downscale(inputBuffer) {
  this.cacheable()
  const cb = this.async()
  processImageCb.call(this, inputBuffer, cb)
}

module.exports.raw = true
