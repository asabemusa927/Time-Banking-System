# Time Banking System Smart Contract

This smart contract implements a decentralized time banking system on the Stacks blockchain. Users can offer services, request services from others, and trade time as currency.

## Overview

Time banking is a reciprocity-based work trading system where time is the principal currency. For every hour participants deposit in a time bank by giving services to others, they can withdraw equivalent hours of service from other members.

## Features

- User registration with initial time credits
- Service registration and management
- Service request and approval workflow
- Reputation system for users
- Time-based currency exchange

## Contract Functions

### User Management

- `register-user`: Register a new user with initial time credits
- `get-user`: Get user details
- `get-user-balance`: Get a user's time credit balance
- `get-user-reputation`: Get a user's reputation score

### Service Management

- `register-service`: Register a new service offering
- `update-service`: Update an existing service
- `get-service`: Get service details

### Transaction Management

- `request-service`: Request a service from a provider
- `approve-service-request`: Provider approves a service request
- `complete-service`: Mark a service as completed and transfer time credits
- `cancel-service-request`: Cancel a pending service request
- `get-transaction`: Get transaction details

### Admin Functions

- `set-admin`: Change the contract administrator
- `get-admin`: Get the current administrator

## Usage Examples

### Register as a User

```clarity
(contract-call? .banking-system register-user)
```

### Register a Service

```clarity
(contract-call? .banking-system register-service "Gardening" "Help with garden maintenance and planting" u1)
```

### Request a Service

```clarity
(contract-call? .banking-system request-service u1 u2)
```

### Approve a Service Request

```clarity
(contract-call? .banking-system approve-service-request u1)
```

### Complete a Service

```clarity
(contract-call? .banking-system complete-service u1)
```

## Error Codes

- `ERR_UNAUTHORIZED (u100)`: Unauthorized access
- `ERR_NOT_FOUND (u101)`: Resource not found
- `ERR_INSUFFICIENT_BALANCE (u102)`: Insufficient time credit balance
- `ERR_INVALID_AMOUNT (u103)`: Invalid amount specified
- `ERR_ALREADY_EXISTS (u104)`: Resource already exists
- `ERR_INVALID_SERVICE (u105)`: Invalid service
- `ERR_INVALID_TRANSACTION (u106)`: Invalid transaction state
```
