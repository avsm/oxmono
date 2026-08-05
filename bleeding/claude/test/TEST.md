# Claude Library Architecture Summary

This document summarizes the architecture of the OCaml Eio Claude library located in `../lib`.

## Overview

The Claude library is a high-quality OCaml Eio wrapper around the Claude Code CLI that provides structured JSON streaming communication with Claude. It follows a clean layered architecture with strong typing and comprehensive error handling.

## Core Architecture

The library is organized into several focused modules that work together to provide a complete Claude integration:

### 1. Transport Layer (`Transport`)
- **Purpose**: Low-level CLI process management and communication
- **Key Functions**: 
  - Spawns and manages the `claude` CLI process using Eio's process manager
  - Handles bidirectional JSON streaming via stdin/stdout
  - Provides `send`/`receive_line` primitives with proper resource cleanup
- **Integration**: Forms the foundation for all Claude communication

### 2. Message Protocol Layer

#### Content Blocks (`Content_block`)
- **Purpose**: Defines the building blocks of Claude messages
- **Types**: Text, Tool_use, Tool_result, Thinking blocks
- **Key Features**: Each block type has specialized accessors and JSON serialization
- **Integration**: Used by messages to represent diverse content types

#### Messages (`Message`)
- **Purpose**: Structured message types for Claude communication
- **Types**: User, Assistant, System, Result messages
- **Key Features**: 
  - User messages support both simple strings and complex content blocks
  - Assistant messages include model info and mixed content
  - System messages handle session control
  - Result messages provide conversation metadata and usage stats
- **Integration**: Primary data structures exchanged between client and Claude

#### Control Messages (`Control`)
- **Purpose**: Session management and control flow
- **Key Features**: Request IDs, subtypes, and arbitrary JSON data payload
- **Integration**: Used for session initialization, cancellation, and other operational commands

### 3. Permission System (`Permissions`)
- **Purpose**: Fine-grained control over Claude's tool usage
- **Components**:
  - **Modes**: Default, Accept_edits, Plan, Bypass_permissions
  - **Rules**: Tool-specific permission specifications
  - **Callbacks**: Custom permission logic with context and suggestions
  - **Results**: Allow/Deny decisions with optional modifications
- **Integration**: Consulted by client before allowing tool invocations

### 4. Configuration (`Options`)
- **Purpose**: Session configuration and behavior control
- **Features**:
  - Tool allow/disallow lists
  - System prompt customization (replace or append)
  - Model selection and thinking token limits
  - Working directory and environment variables
- **Integration**: Passed to transport layer and used throughout the session
- **Pattern**: Builder pattern with `with_*` functions for immutable updates

### 5. Client Interface (`Client`)
- **Purpose**: High-level API for Claude interactions
- **Key Functions**:
  - Session creation and management
  - Message sending (`query`, `send_message`, `send_user_message`)
  - Response streaming (`receive`, `receive_all`)
  - Permission discovery and callback management
- **Integration**: Orchestrates all other modules to provide the main user API

### 6. Main Module (`Claude`)
- **Purpose**: Public API facade with comprehensive documentation
- **Features**: 
  - Re-exports all sub-modules
  - Extensive usage examples and architectural documentation
  - Logging configuration guidance
- **Integration**: Single entry point for library users

## Data Flow

1. **Configuration**: Options are created with desired settings
2. **Transport**: Client creates transport layer with CLI process
3. **Message Exchange**: 
   - User messages are sent via JSON streaming
   - Claude responses are received as streaming JSON
   - Messages are parsed into strongly-typed structures
4. **Permission Checking**: Tool usage is filtered through permission system
5. **Content Processing**: Response content blocks are extracted and processed
6. **Session Management**: Control messages handle session lifecycle

## Key Design Principles

- **Eio Integration**: Native use of Eio's concurrency primitives (Switch, Process.mgr)
- **Type Safety**: Comprehensive typing with specific error exceptions
- **Streaming**: Efficient processing via `Message.t Seq.t` sequences
- **Modularity**: Clear separation of concerns with minimal inter-dependencies
- **Documentation**: Extensive interface documentation with usage examples
- **Error Handling**: Specific exception types for different failure modes
- **Logging**: Structured logging with per-module sources using the Logs library

## Usage Patterns

The library supports both simple text queries and complex multi-turn conversations:

- **Simple Queries**: `Client.query` with text input
- **Tool Control**: Permission callbacks and allow/disallow lists
- **Streaming**: Process responses as they arrive via sequences
- **Session Management**: Full control over Claude's execution environment
- **Custom Prompts**: System prompt replacement and augmentation

The architecture enables fine-grained control over Claude's capabilities while maintaining ease of use for common scenarios.