import { mainnet, polygon } from 'viem/chains';
import { type Address } from 'viem';

export const chain = polygon;

export const bundlerUrl: Record<number, string> = {
  [chain.id]: process.env.BUNDLER_URL!,
};

export const paymasterUrl: Record<number, string> = {
  [chain.id]: process.env.PAYMASTER_URL!,
};

export const chainToFeeToken: Record<number, Address> = {
  [chain.id]: process.env.FEE_TOKEN_ADDRESS as Address,
};
