
const express = require('express');
const path = require('path');
const server = express();

server.use(express.static(path.join(__dirname, 'public')));

const PORT = 3000;
server.listen(PORT, () => {
  console.log(`Listening on port ${PORT}`);
});
