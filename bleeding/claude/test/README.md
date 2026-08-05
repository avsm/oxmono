# Claude IO Test Suite

This directory contains test programs for the Claude IO OCaml library.

## Available Tests

### camel_jokes
A fun demonstration that runs three concurrent Claude instances to generate camel jokes.
Tests concurrent client handling and basic message processing.

### permission_demo  
An interactive demonstration of Claude's permission system.
Shows how to implement custom permission callbacks and grant/deny access to tools dynamically.

## Running Tests

```bash
# Run the camel joke competition
dune exec camel_jokes

# Run the permission demo (interactive)
dune exec permission_demo

# With verbose output to see message flow
dune exec permission_demo -- -v
```

## Features Tested

- Concurrent Claude client instances
- Message handling and processing
- Permission callbacks
- Tool access control
- Typed message API
- Pretty printing of messages