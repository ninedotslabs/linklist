const path = require('path')
const fs = require('fs')

// load json files
const readDirectoryPath = path.join(__dirname, 'public', 'assets', 'data')
const files = fs.readdirSync(readDirectoryPath)
const profiles = files.map((file) => {
  const data = JSON.parse(
    fs.readFileSync(`${path.join(__dirname, 'public', 'assets', 'data', file)}`, 'utf8'),
  )
  return data
})


// generate list file
const writeDirectoryPath = path.join(__dirname, 'public', 'list.json')
const output = profiles

fs.writeFileSync(writeDirectoryPath, JSON.stringify(output))
