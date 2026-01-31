#!/usr/bin/env python3
# /// script
# requires-python = ">=3.9"
# dependencies = [
#     "claude-code-sdk",
# ]
# ///
"""
Permission demo for Claude Code SDK Python.
Demonstrates how the permission callback system works.
"""

import asyncio
import sys
import logging
from typing import Any, Dict

from claude_code_sdk import ClaudeSDKClient, ClaudeCodeOptions
from claude_code_sdk.types import (
    PermissionResultAllow,
    PermissionResultDeny,
    ToolPermissionContext,
)

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

# Track granted permissions
granted_permissions = set()


async def interactive_permission_callback(
    tool_name: str,
    tool_input: Dict[str, Any],
    context: ToolPermissionContext
) -> PermissionResultAllow | PermissionResultDeny:
    """Interactive permission callback that asks user for permission."""
    
    logger.info(f"🔔 Permission callback invoked for tool: {tool_name}")
    print(f"\n🔐 PERMISSION REQUEST 🔐")
    print(f"Tool: {tool_name}")
    
    # Log the full input for debugging
    logger.info(f"Full input: {tool_input}")
    
    # Show input details
    try:
        if tool_name == "Read":
            file_path = tool_input.get("file_path", "")
            print(f"File: {file_path}")
        elif tool_name == "Bash":
            command = tool_input.get("command", "")
            print(f"Command: {command}")
        elif tool_name in ["Write", "Edit"]:
            file_path = tool_input.get("file_path", "")
            print(f"File: {file_path}")
        elif tool_name == "Glob":
            pattern = tool_input.get("pattern", "")
            path = tool_input.get("path", "(current directory)")
            print(f"Pattern: {pattern}")
            print(f"Path: {path}")
        elif tool_name == "Grep":
            pattern = tool_input.get("pattern", "")
            path = tool_input.get("path", "(current directory)")
            print(f"Pattern: {pattern}")
            print(f"Path: {path}")
        else:
            print(f"Input: {tool_input}")
    except Exception as e:
        logger.info(f"Failed to parse input details: {e}")
    
    # Check if already granted
    if tool_name in granted_permissions:
        print("→ Auto-approved (previously granted)")
        logger.info(f"Returning allow result for {tool_name}")
        return PermissionResultAllow()
    
    # Ask user
    response = input("Allow? [y/N/always]: ").lower().strip()
    
    if response in ["y", "yes"]:
        print("→ Allowed (this time only)")
        logger.info(f"User approved {tool_name} for this request only")
        return PermissionResultAllow()
    elif response in ["a", "always"]:
        granted_permissions.add(tool_name)
        print(f"✅ Permission granted for: {tool_name}")
        logger.info(f"User granted permanent permission for {tool_name}")
        return PermissionResultAllow()
    else:
        print(f"❌ Permission denied for: {tool_name}")
        logger.info(f"User denied permission for {tool_name}")
        return PermissionResultDeny(
            message=f"User denied access to {tool_name}",
            interrupt=False
        )


async def run_demo():
    """Run the permission demo."""
    print("🚀 Starting Permission Demo")
    print("==================================")
    print("This demo starts with NO permissions.")
    print("Claude will request permissions as needed.\n")
    
    # Create options with custom permission callback
    # Test WITHOUT allowed_tools to see if permission requests come through
    options = ClaudeCodeOptions(
        model="sonnet",
        # allowed_tools=["Read", "Write", "Bash", "Edit", "Glob", "Grep"],
        can_use_tool=interactive_permission_callback,
    )
    
    async with ClaudeSDKClient(options=options) as client:
        # First prompt - Claude will need to request Read permission
        print("\n📤 Sending first prompt (reading from ../lib)...")
        messages = []
        await client.query(
            "Please read and analyze the source files in the ../lib directory. "
            "Focus on the main OCaml modules and their purpose. "
            "What is the overall architecture of this Claude library?"
        )
        
        async for msg in client.receive_response():
            messages.append(msg)
            if hasattr(msg, 'content'):
                if isinstance(msg.content, str):
                    print(f"\n📝 Claude says:\n{msg.content}")
                elif isinstance(msg.content, list):
                    for block in msg.content:
                        if hasattr(block, 'text'):
                            print(f"\n📝 Claude says:\n{block.text}")
        
        # Show current permissions
        print("\n📋 Current permission status:")
        if granted_permissions:
            print(f"Currently granted permissions: {', '.join(granted_permissions)}")
        else:
            print("No permissions granted yet")
        
        # Second prompt - will need Write permission
        print("\n📤 Sending second prompt (writing TEST.md)...")
        await client.query(
            "Now write a summary of what you learned about the Claude library "
            "architecture to a file called TEST.md in the current directory. "
            "Include the main modules, their purposes, and how they work together."
        )
        
        async for msg in client.receive_response():
            if hasattr(msg, 'content'):
                if isinstance(msg.content, str):
                    print(f"\n📝 Claude says:\n{msg.content}")
                elif isinstance(msg.content, list):
                    for block in msg.content:
                        if hasattr(block, 'text'):
                            print(f"\n📝 Claude says:\n{block.text}")
        
        # Show final permissions
        print("\n📋 Final permission status:")
        if granted_permissions:
            print(f"Currently granted permissions: {', '.join(granted_permissions)}")
        else:
            print("No permissions granted yet")
    
    print("\n==================================")
    print("✨ Demo complete!")


async def main():
    """Main entry point."""
    try:
        await run_demo()
    except KeyboardInterrupt:
        print("\n\nDemo interrupted by user.")
    except Exception as e:
        logger.error(f"Error in demo: {e}", exc_info=True)
        sys.exit(1)


if __name__ == "__main__":
    asyncio.run(main())