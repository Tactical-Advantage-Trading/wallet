import type { Request, Response } from 'express';
import { parseUnits } from 'viem';

import { buildTransferERC20Tx as buildTransferTx, getNexusClient } from '../lib/biconomy_nexus';
import { chain, chainToFeeToken } from '../lib/config';
import { txQueue } from '../lib/txQueue';

export default async function executeTransactionHandler(req: Request, res: Response): Promise<any> {
  try {
    const { pk, recipient, amount } = req.body;
    const feeTokenAddress = chainToFeeToken[chain.id];
    const { nexusClient, paymaster } = await getNexusClient(pk);

    const amountBig = BigInt(amount);
    const call = buildTransferTx(feeTokenAddress, recipient, amountBig);
    const userOp = await nexusClient.prepareUserOperation({ 
      calls: [call]
    });

    const quote = await paymaster.getTokenPaymasterQuotes({ 
      userOp, tokenList: [feeTokenAddress] 
    });

    const usdcFeeAmount = parseUnits(
      quote.feeQuotes[0].maxGasFee.toString(), 
      quote.feeQuotes[0].decimal
    );

    const customApprovalAmount = BigInt(amount) + BigInt(usdcFeeAmount);
    const userOpHash = await nexusClient.sendTokenPaymasterUserOp({ 
      calls: [call], feeTokenAddress, customApprovalAmount 
    });

    txQueue.push({ userOpHash, nexusClient });
    res.json({ userOpHash });
  } catch (error) {
    console.error(error);
    res.status(500).json(error);
  }
}
