import type { NexusClient } from '@biconomy/abstractjs';
import type { Hex } from 'viem';

type UserOpContext = {
  userOpHash: Hex;
  nexusClient: NexusClient;
};

class TxQueue {
  private queue: UserOpContext[] = [];

  private resolvers: ((value: UserOpContext) => void)[] = [];

  push(txContext: UserOpContext) {
    if (this.resolvers.length) {
      const resolve = this.resolvers.shift()!;
      resolve(txContext);
    } else {
      this.queue.push(txContext);
    }
  }

  async pop(): Promise<UserOpContext> {
    if (this.queue.length) {
      return this.queue.shift()!;
    }

    return new Promise((resolve) => {
      this.resolvers.push(resolve);
    });
  }
}

export const txQueue = new TxQueue();
