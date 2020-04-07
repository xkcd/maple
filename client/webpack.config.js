const path = require('path')
const webpack = require('webpack')
const CopyWebpackPlugin = require('copy-webpack-plugin')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const ZipWebpackPlugin = require('zip-webpack-plugin')

const comicData = require('./comic')

const tsLoader = {
  test: /\.tsx?$/,
  use: 'ts-loader',
  exclude: /node_modules/,
}

function comicImageLoader(mode) {
  return {
    test: /\.png$/,
      use: [
        mode === 'development' && 'cache-loader',
        {
          loader: 'comic-image-loader',
          options: {
            name: 'static/[contenthash:6].[ext]',
            quant: true,
            scale: .1,
          },
        },
      ].filter(Boolean),
  }
}

const resolve = {
  extensions: ['.tsx', '.ts', '.js'],
}

const resolveLoader = {
  modules: [
    'node_modules',
    path.resolve(__dirname, 'loaders'),
  ]
}

function devServer() {
  return {
    writeToDisk: true,
    contentBase: path.resolve(__dirname, 'dist'),
    proxy: {
      '/api': {
        target: process.env.MAPLE_API_SERVER,
        pathRewrite: {'^/api' : ''},
        changeOrigin: true,
        headers: {
          'Origin': new URL(process.env.MAPLE_API_SERVER).origin,
        },
      },
    },
  }
}

function commonPlugins(mode) {
  return [
    new webpack.BannerPlugin('code by chromako.de.'),
    new webpack.DefinePlugin({
      'process.env.MAPLE_API_SERVER': JSON.stringify(mode === 'development' ? '/api/' : process.env.MAPLE_API_SERVER),
      'process.env.MAPLE_ITEM_IMG_URL': JSON.stringify(process.env.MAPLE_ITEM_IMG_URL),
      'process.env.USE_MOCK_API': JSON.stringify(process.env.USE_MOCK_API),
    }),
  ]
}

function buildComic(env, argv) {
  const publicPath = process.env.MAPLE_ASSET_URL || '/comic/'
  return {
    name: 'comic',
    entry: {
      comic: './src/index.tsx',
    },
    output: {
      path: path.resolve(__dirname, 'dist/comic'),
      filename: '[name].js',
      publicPath,
    },
    module: {
      rules: [
        tsLoader,
        comicImageLoader(argv.mode),
      ],
    },
    resolve,
    resolveLoader,
    plugins: [
      ...commonPlugins(argv.mode),
      new HtmlWebpackPlugin({
        inject: false,
        minify: false,
        template: 'src/index.ejs',
        templateParameters: (compilation, assets, assetTags, options) => ({
          tags: assetTags,
          comic: comicData,
        }),
      }),
    ],
    devServer: argv.mode === 'development' ? devServer() : {},
  }
}

function buildAdmin(env, argv) {
  const publicPath = process.env.MAPLE_ADMIN_ASSET_URL || '/admin/'
  return {
    name: 'admin',
    entry: {
      admin: './src/admin/index.tsx',
    },
    output: {
      path: path.resolve(__dirname, 'dist/admin'),
      filename: '[name].js',
      publicPath,
    },
    module: {
      rules: [
        tsLoader,
        comicImageLoader(argv.mode),
      ],
    },
    resolve,
    resolveLoader,
    plugins: [
      ...commonPlugins(argv.mode),
      new HtmlWebpackPlugin({
        inject: false,
        minify: false,
        template: 'src/admin/index.ejs',
        templateParameters: (compilation, assets, assetTags, options) => ({
          tags: assetTags,
          comic: comicData,
        }),
      }),
    ],
    devServer: argv.mode === 'development' ? devServer(): {},
  }
}

function buildLootExtension(env, argv) {
  const output = {
    path: path.resolve(__dirname, 'dist/extension'),
    filename: '[name].js',
  }

  return {
    name: 'extension',
    entry: {
      extension: ['./src/extension/index.ts', './src/index.tsx'],
    },
    output,
    module: {
      rules: [
        tsLoader,
        comicImageLoader(argv.mode),
      ],
    },
    resolve,
    resolveLoader,
    plugins: [
      ...commonPlugins(argv.mode),
      new CopyWebpackPlugin([
        {
          from: 'src/extension/manifest.json',
          to: 'manifest.json',
          transform: (content) => content.toString().replace('${apiServerURL}', comicData.apiServerURL)
        },
      ]),
      new ZipWebpackPlugin({
        path: '../',
        filename: 'xkcd-loot-extension.zip'
      }),
    ],
  }
}

module.exports = [buildComic, buildAdmin, buildLootExtension]
