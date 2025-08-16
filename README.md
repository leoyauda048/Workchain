# 🤝 Workchain: On-Chain Employment Contracts

A decentralized employment contract system built on Stacks blockchain using Clarity smart contracts.

## 🎯 Features

- Create employment contracts with detailed terms
- Digital signatures for both employer and employee
- Track active and terminated contracts
- Transparent contract terms and conditions
- Immutable employment history

## 📋 Contract Functions

### For Employers

- `create-contract`: Create a new employment contract
- `get-employer-contracts`: View all contracts created
- `terminate-contract`: End an active contract

### For Employees

- `sign-contract`: Sign and activate a contract
- `get-employee-contracts`: View all associated contracts
- `terminate-contract`: End an active contract

## 🚀 Usage

1. Deploy the contract using Clarinet:
```bash
clarinet deploy
```

2. Create a new contract as an employer:
```bash
clarinet contract-call .workchain create-contract 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM' "Software Engineer" u100000 u1672531200 u1704067200 "Development and maintenance" "Health insurance, 401k"
```

3. Sign contract as an employee:
```bash
clarinet contract-call .workchain sign-contract u0
```

## 🔍 Contract Details

- Salary must be greater than 0
- End date must be after start date
- Both parties can terminate the contract
- Contract becomes active only after both signatures
- Each party can have up to 20 contracts tracked

## 🔐 Security

- Only authorized parties can sign or terminate contracts
- Contract terms are immutable once created
- All actions are recorded on-chain


