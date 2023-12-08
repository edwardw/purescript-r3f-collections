const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const fs = require('fs').promises

const CHAPTER_PREFIX = path.join(__dirname, 'output/')

module.exports = async () => {
  const { generatedEntries, generatedPlugins } = await getDirectoryEntries()

  return {
    module: {
      rules: [
        {
          test: /\.glsl$/i,
          use: 'raw-loader'
        }
      ]
    },
    mode: 'development',
    entry: {
      ...generatedEntries,
    },
    devtool: 'inline-source-map',
    devServer: {
      static: [
        {
          directory: path.join(__dirname, 'assets'),
          publicPath: '/assets'
        },
        {
          directory: path.join(__dirname, 'dist')
        }
      ]
    },
    plugins: [
      ...generatedPlugins
    ],
    output: {
      filename: 'js/[name].js',
      library: {
        name: 'LearnThreejs',
        type: 'var'
      }
    },
    optimization: {
      splitChunks: {
        chunks: 'all'
      }
    },
    experiments: {
      asyncWebAssembly: true
    }
  }
}

const getDirectoryEntries = async () => {
  const changeCase = await import("change-case")
  const generatedPlugins = []
  const generatedEntries = {}

  const entries = await fs.readdir(CHAPTER_PREFIX, {
    withFileTypes: true
  })
  const candidates = entries.filter((entry) => entry.name.startsWith("Stories."))

  for (const candidate of candidates) {
    // Assuming every purescript module `Stories.[ChapterName].[StoryName]` is
    // indeed an individual story.
    let [_, chapter, section] = candidate.name.split('.')
    let name = path.parse(section).name
    chapter = changeCase.kebabCase(chapter, { separateNumbers: true })
    name = changeCase.kebabCase(name, { separateNumbers: true })
    const plugin = new HtmlWebpackPlugin({
      filename: chapter + '/' + name + '.html',
      chunks: [name],
      template: 'template.html',
      scriptLoading: 'blocking'
    })

    generatedEntries[name] = {
      import: CHAPTER_PREFIX + '/' + candidate.name + '/index.js'
    }

    generatedPlugins.push(plugin)
  }

  return { generatedEntries, generatedPlugins }
}

