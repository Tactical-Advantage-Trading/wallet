import { buildTransferERC20Tx, getNexusClient } from '../lib/biconomy_nexus';
import { chain, chainToFeeToken } from '../lib/config';
import { type Request, type Response } from 'express';

export default async function estimateGasHandler(req: Request, res: Response): Promise<any> {
  try {
    const token = chainToFeeToken[chain.id]
    const { pk, recipient, amount } = req.body;
    const { nexusClient, paymaster } = await getNexusClient(pk);

    const call = buildTransferERC20Tx(token, recipient, BigInt(amount));
    const userOp = await nexusClient.prepareUserOperation({ 
      calls: [call] 
    });

    const quote = await paymaster.getTokenPaymasterQuotes({ 
      userOp, tokenList: [token]
    });

    const feeAmount = quote.feeQuotes[0].maxGasFee.toString();
    res.json({ feeQuotes: quote.feeQuotes, feeAmount });
  } catch (error) {
    console.error(error);
    res.status(500).json(error);
  }
}
