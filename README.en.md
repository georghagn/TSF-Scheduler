|<sub>🇩🇪 [German translation →](README.de.md)</sub>|
|----:|
|    |

|[![Pharo Version](https://img.shields.io/badge/Pharo-12.0%2B-blue.svg)](https://pharo.org)|[![License](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE) [![Dependencies](https://img.shields.io/badge/dependencies-zero-brightgreen.svg)](#)|
|----|----|
|![TSF-Scheduler Logo](logo-scheduler.png)| ***TSF-Scheduler***<br>A robust, thread-safe, lightweight and zero-dependency task scheduling framework for Smalltalk. Part of the **TSF (Tiny Smalltalk Framework)** suite.|

<sup>***TSF*** stands for ***Tiny Smalltalk Framework*** — a collection of minimalist tools for robust applications.</sup>

## Overview

`TSF-Scheduler` provides a powerful mechanism to handle asynchronous background jobs and periodic tasks. It is designed with stability and "Smalltalk-way" usability in mind, distinguishing clearly between **scheduling logic** (Timer) and **execution logic** (Worker).

## Key Features

* **Thread-Safe:** Uses *`Mutex`* for queue protection and atomic operations.
* **Architecture:** Separation of concerns with *`TsfCron`* (Timer) and *`TsfScheduler`* (Worker).
* **Dual Mode:** Supports both **Blocks** (for quick scripting) and **Subclasses** (for clean Command Pattern architecture).
* **Smart Scheduling:** Uses a **Fixed Delay** strategy for periodic tasks to prevent execution stacking (a task is rescheduled only *after* it finishes).
* **Idempotency:** Safely re-evaluate and update task configurations at runtime without duplicating jobs.
* **Lifecycle Management:** Support for *`pause`*, *`resume`*, and *`cancel`* on running tasks.
* **Graceful Shutdown:** Cooperative thread termination ensures resources are not left in inconsistent states.

## Installation

```smalltalk
Metacello new
    baseline: 'TsfScheduler';
    repository: 'github://georghagn/TSF-Scheduler:main';
    load.
```

## Usage

### 1. Starting the System

The system consists of the Scheduler (executes tasks) and the Cron (manages time).

```smalltalk
TsfScheduler current start.
TsfCron current start.
```

You can also use the convenient Method from the *`TSF-System`* 
```smalltalk
TsfSystem class >> start
```

### 2. One-Off Tasks (Fire and Forget)

For simple background operations, use blocks directly with the scheduler.

```smalltalk
TsfScheduler current scheduleBlock: [ 
    (Delay forSeconds: 2) wait.
    Transcript show: 'Background job finished!'; cr.
].
```

### 3. Periodic Tasks (The "Scripting" Way)

You can schedule tasks that repeat. Use the ensureTaskNamed: method to prevent duplicate tasks when re-running scripts.

```smalltalk
TsfCron current 
    ensureTaskNamed: 'System Cleanup' 
    frequency: 10 minutes
    action: [ 
        Transcript show: 'Running cleanup...'; cr.
        "Cleanup logic here"
    ].
```

### 4. Periodic Tasks (The "Robust" Way)

For complex logic, avoid large blocks. Instead, subclass TsfTask and implement executeAction.

**Define the class:**

```smalltalk
TsfTask subclass: #LogRotationTask
    instanceVariableNames: ''
    package: 'MyApp-Maintenance'
```

**Implement the logic:**

```smalltalk
LogRotationTask >> executeAction
    Transcript show: 'Rotating logs...'; cr.
    "Complex logic goes here, e.g., file access, compression"
```

**Schedule it:**

```smalltalk
TsfCron current 
    ensureTask: 'Log Rotation' 
    class: LogRotationTask 
    frequency: 1 hour.
```

### 5. Lifecycle Control

You can control tasks even after they have been scheduled.

```smalltalk
| task |
task := TsfCron current findTaskByName: 'System Cleanup'.

task pause.   "Stops execution, but keeps the timer ticking"
task resume.  "Resumes execution"
task cancel.  "Permanently stops and removes the task"
```

## Architecture

- **TsfScheduler:** A Singleton worker that consumes a queue of tasks. It has no concept of time, only work. It processes tasks sequentially in a background process.
- **TsfCron:** A Singleton timer that manages a priority queue of periodic tasks. It wakes up only when a task is due or a new task is inserted (Interruptible Wait), ensuring 0% CPU usage when idle.

## Error Handling

Tasks catch their own errors to prevent crashing the worker thread. You can define per-task handlers or a global handler.

```smalltalk
TsfScheduler current globalErrorHandler: [ :task :error |
    Transcript 
		show: 'Critical failure in ';
		showCr: task name.
].
```
## Development-Process & Credits

Special thanks go to my AI sparring partner for the intensive and valuable discussions during the design phase. The AI's ability to quickly outline different architectural approaches (such as polling loops vs. priority queues) and weigh their pros and cons significantly accelerated the development of `tsf-scheduler` and improved the robustness of the final result.


## License

MIT

## Contact

If you have any questions or are interested in this project, you can reach me at   
📧 *dev.georgh [at] hconsult.biz*

<sup>*(Please do not send inquiries to the private GitHub account addresses.)*</sup>





