# pms-infrastructure

`pms-infrastructure` is one of the internal packages that make up the `pty-mcp-server` project.  
It provides concrete implementations for accessing external systems, such as file storage, environment variables, and time, and acts as the boundary between the core application logic and the outside world.

In this context, "outside world" refers specifically to the Linux environment in which the server runs.  
By connecting to external systems via a pseudoterminal (pty), this package enables the AI to interface with the operating system as if it were a human user.  
It sends messages through the pty to simulate terminal interactions, thereby granting the AI controlled access to the shell and system-level tools.

This architecture allows the MCP server to act as a mediator between structured internal commands and real-world system behavior.

---

## Package Structure

---

## Module Structure

---
