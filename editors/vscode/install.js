const package_json = require("./package.json");
require("child_process")
  .execSync(`code --install-extension ${package_json.name}-${package_json.version}.vsix`);

const cmd = require("child_process")
  .spawn("code", ["--install-extension", `${package_json.name}-${package_json.version}.vsix`]);

cmd.stdout.on('data', data => console.log(data.toString()));
cmd.stderr.on('data', data => console.error(data.toString()));
