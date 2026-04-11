# Slix 0.0.1 Specification

## 1. Overview

Slix is a small, composable operating system architecture based on handles, services, and explicit process construction.

This specification defines the **core system interface and behavioral contract** for Slix 0.0.1. It is intentionally implementation-independent, but sufficiently precise to guide implementation.

Conforming systems MUST provide equivalent observable behavior.

---

## 2. Design Principles

* **Uniformity**: All resources are accessed via handles.
* **Explicitness**: Process environments are constructed explicitly.
* **Composability**: Systems are built from services.
* **Portability**: The API MUST support both desktop and embedded profiles.
* **POSIX-compatibility (semantic)**: Behavior MUST be sufficient to support a POSIX compatibility layer.

---

## 3. Core Concepts

### 3.1 Handles

A handle is an opaque, process-local reference to a resource.

Properties:

* Handles are integer-like values.
* Handles are not globally meaningful.
* Handles refer to shared underlying resources.
* Multiple handles MAY refer to the same resource.

### 3.2 Services

Resources are provided by services.

* Services MAY be separate processes (desktop profile).
* Services MAY be in-process modules (embedded profile).

### 3.3 Processes

A process is an execution context with:

* its own handle table
* its own address space (desktop profile)

---

## 4. System Interface

### 4.1 Types

```c
typedef int32_t handle_t;
typedef int64_t ssize_t;
typedef int64_t off_t;
```

Return conventions:

* >= 0: success
* < 0: error code

---

### 4.2 read

```c
ssize_t read(handle_t h, void* buf, size_t len);
```

#### Semantics

* MUST block until:

  * data is available, OR
  * EOF is reached, OR
  * an error occurs

* MUST return:

  * > 0: number of bytes read
  * 0: EOF
  * < 0: error

* MAY return fewer bytes than requested.

#### EOF Invariant

Once EOF is reached, all subsequent reads MUST return 0.

---

### 4.3 write

```c
ssize_t write(handle_t h, const void* buf, size_t len);
```

#### Semantics

* MUST block until:

  * at least one byte can be written, OR
  * an error occurs

* MUST return:

  * > 0: number of bytes written
  * < 0: error

* MAY perform partial writes.

#### Broken Pipe

If writing to a pipe with no readers, MUST return an error.

---

### 4.4 close

```c
int close(handle_t h);
```

#### Semantics

* Releases the handle.
* Further use MUST return an error.

---

### 4.5 seek

```c
off_t seek(handle_t h, off_t offset, int whence);
```

#### Whence Values

```c
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
```

#### Semantics

* MUST succeed for seekable handles.
* MUST fail for non-seekable handles.

---

### 4.6 poll

```c
typedef struct {
  handle_t h;
  int events;   // input: requested events
  int revents;  // output: returned events
} pollfd_t;

int poll(pollfd_t* fds, int count, int timeout);
```

#### Event Flags

```c
#define POLL_READABLE 1
#define POLL_WRITABLE 2
#define POLL_ERROR    4
#define POLL_HANGUP   8
```

#### Semantics

* MUST block until:

  * one or more handles become ready, OR
  * timeout expires

* On return, each `fds[i].revents` MUST contain the ready event flags.

#### Readiness

* READABLE: read() will not block
* WRITABLE: write() will not block for at least one byte
* HANGUP: EOF condition
* ERROR: error condition

---

### 4.7 spawn

```c
pid_t spawn(
  const char* path,
  char* const argv[],
  char* const env[],
  handle_t handles[],
  size_t handle_count
);
```

#### Semantics

* Creates a new process.
* The child handle table MUST be initialized from the provided handles.
* No implicit inheritance MUST occur.

#### Inheritance Rules

* Handles are copied by reference to the same underlying resource.
* Parent and child handle tables are independent.

#### Standard Handles

Implementations SHOULD treat:

* 0 as stdin
* 1 as stdout
* 2 as stderr

#### Exit Behavior

* A process terminates by returning from its entry point or explicitly exiting.
* Each process MUST produce an integer exit code.

---

### 4.8 wait

```c
int wait(pid_t pid);
```

#### Semantics

* MUST block until the process exits.
* MUST return the exit code.

---

### 4.9 connect

```c
handle_t connect(const char* name);
```

#### Semantics

* Resolves a resource name to a handle.
* MUST return a valid handle or an error.

#### Naming

Names MUST follow:

```
<scheme>:<path-or-spec>
```

#### Required Schemes (Desktop Profile)

* `vfs:` — file system access
* `tty:` — terminal devices
* `dev:` — device access

#### Service Discovery

* Implementations MUST provide a mechanism to discover available services and schemes.
* A system MAY expose a well-known service registry (e.g., `svc:`) or a virtual namespace under `vfs:`.
* Services MAY register custom schemes.

#### Resolution Rules

* If the scheme is not recognized, connect() MUST fail.
* If the resource cannot be opened or created, connect() MUST fail.
* Services MAY interpret the path-or-spec according to their own rules.

#### Embedded Profile

* MAY support a reduced or static mapping.
* MAY implement connect() as a fixed lookup table.

---

### 4.10 create_pipe

```c
int create_pipe(handle_t* read_end, handle_t* write_end);
```

#### Semantics

* Creates a unidirectional byte stream.

---

### 4.11 dup

```c
handle_t dup(handle_t h);
```

#### Semantics

* Returns a new handle referring to the same resource.

---

### 4.12 dup2

```c
int dup2(handle_t src, handle_t dst);
```

#### Semantics

* Replaces dst with src.
* If dst is open, it MUST be closed first.

---

### 4.13 stat

```c
int stat(handle_t h, struct stat* out);
```

#### Semantics

* Retrieves metadata about a handle.
* MUST fill out the stat structure or return an error.

#### Required Fields (minimum)

```c
struct stat {
  uint64_t size;
  uint32_t type;
  uint32_t flags;  // bitmask: seekable, readable, writable
};
```

* Implementations MAY extend this structure.

#### Type Constants

```c
#define STAT_FILE    0
#define STAT_DIR     1
#define STAT_PIPE    2
#define STAT_TTY     3
#define STAT_DEVICE  4
```

* Implementations MUST use these values for the `type` field.
* Implementations MAY define additional type values starting at 128.

---

## 5. Handle Model

### 5.1 Lifetime

* Handles reference shared resources.
* Closing the last handle MAY release the resource.

### 5.2 Reference Semantics

* dup() and spawn() create additional references.

### 5.3 Limits

* Implementations SHOULD support a minimum number of handles per process.
* Implementations MAY impose a maximum handle count.

---

## 6. I/O Semantics

### 6.1 Blocking

* read MUST block until data, EOF, or error.
* write MUST block until at least one byte can be written or an error occurs.

### 6.2 Partial Operations

* read and write MAY complete partially.

### 6.3 Non-blocking (Optional)

* Implementations MAY support non-blocking operation.
* In non-blocking mode, operations that would block MUST return -EAGAIN.

---

## 7. Pipes

* read blocks if empty.
* If all writers are closed, read MUST return 0.
* write blocks if buffer full.
* write MUST fail if no readers exist.

---

## 8. TTY (Minimal Contract)

* TTY handles MUST support read and write.
* TTY MAY implement line buffering and echo.

---

## 9. Error Model

Errors MUST be returned as negative values. Error codes MUST be stable and consistent across the system.

### 9.1 Required Error Codes

Implementations MUST provide at least the following error conditions:

* `-EBADF`   — Invalid handle
* `-EINVAL`  — Invalid argument
* `-EAGAIN`  — Operation would block (non-blocking mode)
* `-EPIPE`   — Broken pipe (write with no readers)
* `-ESPIPE`  — Illegal seek on non-seekable handle
* `-ENOSYS`  — Operation not implemented

Implementations SHOULD also provide:

* `-ENOMEM`  — Out of memory
* `-EACCES`  — Permission denied
* `-ENOENT`  — Resource not found

### 9.2 General Rules

* Functions MUST NOT return both data and an error simultaneously.
* A return value < 0 indicates failure; >= 0 indicates success.

---

## 10. Profiles

### Desktop Profile

* Multi-process
* VFS
* Networking

### Embedded Profile

* MAY omit VFS
* MAY use single-process
* MUST preserve API semantics

---

## 11. Conformance

A system conforms to Slix 0.0.1 if it implements all REQUIRED behavior defined in this document.

---

## 12. Non-Normative Examples

### 12.1 Pipe and Spawn

```c
handle_t r, w;
create_pipe(&r, &w);

handle_t child_handles[3];
child_handles[0] = r;
child_handles[1] = /* stdout */;
child_handles[2] = /* stderr */;

spawn("/bin/consumer", argv, env, child_handles, 3);
```

### 12.2 Output Redirection

```c
handle_t file = connect("vfs:/out.txt");

handle_t child_handles[3];
child_handles[0] = /* stdin */;
child_handles[1] = file;
child_handles[2] = /* stderr */;

spawn("/bin/producer", argv, env, child_handles, 3);
```

---

## 13. Service Communication Model (Informative)

Slix systems are typically implemented using message passing between processes or modules.

### 13.1 Message Structure (Suggested)

```c
struct Message {
  uint32_t type;
  uint32_t handle;
  uint64_t arg0;
  uint64_t arg1;
  void*    buffer;
  int64_t  result;
};
```

### 13.2 Request Types (Examples)

* READ
* WRITE
* CLOSE
* SEEK
* CONNECT

### 13.3 Message Handling Model

For any operation (READ, WRITE, etc.):

1. The caller issues a request using a handle.
2. The system routes the request based on endpoint.
3. The service processes the request.
4. The service returns a result.

Blocking behavior corresponds to waiting for this response.

### 13.4 Embedded Profile

* Message passing MAY be implemented as direct function calls.
* Endpoint routing MAY be implemented as a switch or lookup table.

This model is provided as guidance and is not required for conformance.

---

## 14. Service and Endpoint Model

This section defines the normative model for service identification, resource ownership, and message routing.

### 14.1 Services

A service is a component responsible for managing a class of resources and handling requests on those resources.

Properties:

* Services MUST exclusively own and manage their resources.
* Services receive requests and return responses.
* All resource behavior MUST be implemented by services; the kernel or runtime MUST NOT interpret resource-specific semantics.
* Services MAY be implemented as separate processes (desktop profile) or in-process modules (embedded profile).

Examples include:

* VFS service (files)
* TTY service (terminals)
* Device services (hardware)

---

### 14.2 Endpoints

An endpoint is a routable identifier for a service instance.

```c
typedef uint32_t endpoint_t;
```

Properties:

* Each service instance MUST have a unique endpoint.
* Endpoints MUST be unique within the system.
* Endpoints MUST remain stable for the lifetime of the service.
* Each endpoint MUST correspond to exactly one service instance.

---

### 14.3 Handle Binding

Each handle is associated with a service endpoint and a resource identifier.

Conceptually:

```
handle = (endpoint, resource_id)
```

* `endpoint` identifies the service.
* `resource_id` identifies the resource within the service.

Given a handle, the system MUST:

1. Extract the endpoint.
2. Route the request to the corresponding service.
3. Provide the `resource_id` to the service.

The service MUST interpret `resource_id`.

---

### 14.4 Service Registry

Implementations MUST provide a mechanism to resolve service schemes to endpoints.

Conceptually:

```
scheme -> endpoint
```

Requirements:

* Services MUST register one or more schemes.
* A lookup mechanism MUST exist to resolve a scheme to an endpoint.
* Duplicate scheme registration MUST be rejected or otherwise well-defined.
* The registry MUST provide deterministic resolution.

Implementations MAY provide:

* A dedicated registry service (desktop profile)
* A virtual namespace
* A static mapping (embedded profile)

---

### 14.5 connect() Resolution

The connect() function MUST operate as follows:

1. Parse the scheme from the name.
2. Resolve the scheme to an endpoint using the registry.
3. Send a CONNECT message to the service.
4. The service MUST either return a valid resource identifier, or return an error.
5. Construct and return a handle bound to the endpoint and resource.

---

### 14.6 Message Routing

Requests MUST be routed based on the endpoint associated with a handle.

Conceptually:

```
handle -> endpoint -> service
```

* The system MUST deliver requests to the correct service.
* The service MUST interpret the resource identifier.

---

### 14.7 Embedded Profile

In embedded systems:

* Services MAY be compiled into a single address space.
* Message passing MAY be implemented as direct function calls.
* Endpoint routing MAY be implemented as a switch or lookup table.
* The logical model defined in this section MUST still be preserved.
* Handles MUST still conceptually map to `(endpoint, resource_id)`.

---

### 14.8 Design Invariants

The following invariants MUST hold in all conforming implementations:

* All resource access occurs through handles.
* All handles resolve to a service endpoint.
* Services MUST exclusively own their resources; all operations are serviced by the owning service.
* The kernel (or core runtime) MUST NOT interpret resource-specific semantics.
* The API MUST remain independent of the underlying communication mechanism.

---

## 15. Rationale (Non-Normative)

The service and endpoint model enables:

* **Microkernel architecture**: Services run in user space; the kernel provides only routing and isolation.
* **Embedded collapse**: Services compile into a single address space with direct-call dispatch, preserving logical structure without runtime overhead.
* **Capability-based access**: Handles act as unforgeable capabilities; possessing a handle confers authority to use the resource.

This design is inspired by MINIX 3 but simplified and generalized for both desktop and embedded deployment.
