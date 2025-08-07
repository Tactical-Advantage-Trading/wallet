import { type Request, type Response } from 'express';
import { getNexusClient } from '../lib/biconomy_nexus';

export default async function getSmartAccountAddress(req: Request, res: Response): Promise<any> {
  try {
    const { pk } = req.body;
    const { nexusClient } = await getNexusClient(pk);
    res.json({ smartAccountAddress: nexusClient.account.address });
  } catch (error) {
    console.error(error);
    res.status(500).json(error);
  }
}
