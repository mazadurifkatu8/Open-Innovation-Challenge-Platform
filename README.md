# Open Innovation Challenge Platform
 
# Open Innovation Challenge Platform

A decentralized platform for creating and participating in innovation challenges. This smart contract allows organizations to post challenges with rewards, and innovators to submit solutions and earn rewards for winning submissions.

## Features

- Create innovation challenges with STX rewards
- Submit solutions to open challenges
- Select winners for challenges
- Claim rewards for winning solutions
- View challenge and submission details

## Contract Functions

### For Challenge Creators

- `create-challenge`: Create a new innovation challenge with a title, description, and STX reward
- `select-winner`: Choose a winning submission for your challenge
- `close-challenge`: Close a challenge without selecting a winner (refunds the reward)

### For Innovators

- `submit-solution`: Submit a solution to an active challenge
- `claim-reward`: Claim the reward if your submission was selected as the winner

### Read-Only Functions

- `get-challenge`: Get details about a specific challenge
- `get-submission`: Get details about a specific submission
- `get-challenge-submissions`: Get all submission IDs for a challenge
- `get-user-submission`: Check if a user has submitted to a specific challenge

## Usage Examples

### Creating a Challenge

```clarity
(contract-call? .open-inno create-challenge "Sustainable Energy Solution" "Looking for innovative approaches to renewable energy storage" u1000000000)
```

### Submitting a Solution

```clarity
(contract-call? .open-inno submit-solution u1 "https://example.com/my-solution" "A new battery technology using recycled materials")
```

### Selecting a Winner

```clarity
(contract-call? .open-inno select-winner u1 u3)
```

### Claiming a Reward

```clarity
(contract-call? .open-inno claim-reward u1)
```

## Error Codes

- `u100`: Not the contract owner
- `u101`: Challenge or submission not found
- `u102`: Challenge is closed
- `u103`: Already submitted to this challenge
- `u104`: Not the winner of the challenge
- `u105`: Reward already paid
- `u106`: Insufficient funds
- `u107`: Unauthorized action
- `u108`: Challenge is still active
```
