var {compile, serialize, stringify} = require('stylis')

process.stdout.write(serialize(compile(process.argv[2]), stringify))
