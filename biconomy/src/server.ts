import express from 'express';
import http from 'http';
import getSmartAccountAddress from './handlers/getSmartAccountAddress';
import executeTransactionHandler from './handlers/executeTransaction';
import estimateGasHandler from './handlers/estimateGas';

const app = express();
app.use(express.json());
app.post('/get-smart-account-address', getSmartAccountAddress);
app.post('/execute-transaction', executeTransactionHandler);
app.post('/estimate-gas', estimateGasHandler);
const server = http.createServer(app);
server.listen(3000);
