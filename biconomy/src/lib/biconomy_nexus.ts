import { privateKeyToAccount } from 'viem/accounts';
import { type Address, encodeFunctionData, type Hex, http, parseAbi } from 'viem';
import { bundlerUrl, chain, chainToFeeToken, paymasterUrl } from './config';

import {
  createBicoPaymasterClient,
  createNexusClient,
  toBiconomyTokenPaymasterContext,
  toNexusAccount,
} from '@biconomy/abstractjs';

export async function getNexusClient(privateKey: Hex) {
  const paymasterContext = toBiconomyTokenPaymasterContext({
    feeTokenAddress: chainToFeeToken[chain.id],
  });

  const paymaster = createBicoPaymasterClient({
    transport: http(paymasterUrl[chain.id]!)
  });

  const account = privateKeyToAccount(privateKey);

  const nexusClient = createNexusClient({
    account: await toNexusAccount({
      signer: account,
      chain: chain,
      transport: http(),
    }),
    transport: http(bundlerUrl[chain.id]!),
    paymaster,
    paymasterContext,
  });

  return { nexusClient, paymaster };
}

// Build a transaction to transfer <amount> of <token> to <recipient>
export function buildTransferERC20Tx(token: Address, recipient: Address, amount: bigint) {
  return {
    to: token,
    data: encodeFunctionData({
      abi: parseAbi([
        'function transfer(address recipient, uint256 amount) external returns (bool)',
      ]),
      functionName: 'transfer',
      args: [recipient, amount],
    }),
    value: 0n,
  };
}
