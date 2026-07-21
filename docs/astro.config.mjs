// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

export default defineConfig({
	site: 'https://trisc.dev',
	integrations: [
		starlight({
			title: 'TRISC',
			social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/edadma/trisc' }],
			expressiveCode: {
				shiki: {
					langAlias: {
						sysl: 'rust',
					},
				},
			},
			sidebar: [
				{
					label: 'Getting Started',
					items: [
						{ label: 'Introduction', slug: 'getting-started/introduction' },
						{ label: 'Installation', slug: 'getting-started/installation' },
						{ label: 'Hello World', slug: 'getting-started/hello-world' },
					],
				},
				{
					label: 'Tutorials',
					items: [
						{ label: 'Overview', slug: 'tutorials' },
						{ label: 'Your First Sysl Program', slug: 'tutorials/first-program' },
						{ label: 'Memory Modes by Example', slug: 'tutorials/memory-modes' },
						{ label: 'Contracts in Practice', slug: 'tutorials/contracts-in-practice' },
						{ label: 'Generic Combinators', slug: 'tutorials/generic-combinators' },
						{ label: 'Interfaces vs Traits', slug: 'tutorials/interfaces-vs-traits' },
						{ label: 'Targeting Bare Metal', slug: 'tutorials/bare-metal' },
					],
				},
				{
					label: 'Language',
					items: [
						{ label: 'Overview', slug: 'language/overview' },
						{ label: 'Program Structure', slug: 'language/program-structure' },
						{
							label: 'Types',
							items: [
								{ label: 'Overview', slug: 'language/types/overview' },
								{ label: 'Integers & Overflow', slug: 'language/types/integers-and-overflow' },
								{ label: 'Structs', slug: 'language/types/structs' },
								{ label: 'Enums', slug: 'language/types/enums' },
								{ label: 'Type Declarations', slug: 'language/types/type-declarations' },
								{ label: 'Type Attributes', slug: 'language/types/type-attributes' },
								{ label: 'Three Allocation Modes', slug: 'language/types/three-allocation-modes' },
							],
						},
						{ label: 'Variables & Constants', slug: 'language/variables-and-constants' },
						{
							label: 'Functions',
							items: [
								{ label: 'Overview', slug: 'language/functions/overview' },
								{ label: 'Contracts', slug: 'language/functions/contracts' },
								{ label: 'Parameters', slug: 'language/functions/parameters' },
								{ label: 'Def & Pure Functions', slug: 'language/functions/def-pure-functions' },
								{ label: 'Generics', slug: 'language/functions/generics' },
								{ label: 'Try Operator', slug: 'language/functions/try-operator' },
								{ label: 'Traits & Impls', slug: 'language/functions/traits-and-impls' },
								{ label: 'Operator Overloading', slug: 'language/functions/operator-overloading' },
								{ label: 'Interfaces', slug: 'language/functions/interfaces' },
								{ label: 'Methods', slug: 'language/functions/methods' },
								{ label: 'Deinit & Defer', slug: 'language/functions/deinit-and-defer' },
								{ label: 'Function Pointers', slug: 'language/functions/function-pointers' },
								{ label: 'Closures', slug: 'language/functions/closures' },
								{ label: 'Inner Def', slug: 'language/functions/inner-def' },
								{ label: 'Extern', slug: 'language/functions/extern' },
							],
						},
						{ label: 'Expressions', slug: 'language/expressions' },
						{ label: 'Statements', slug: 'language/statements' },
						{ label: 'Arrays, Slices & Pointers', slug: 'language/arrays-slices-pointers' },
						{ label: 'Strings', slug: 'language/strings' },
						{ label: 'Builtins', slug: 'language/builtins' },
						{ label: 'Type Compatibility', slug: 'language/type-compatibility' },
						{ label: 'Runtime Safety', slug: 'language/runtime-safety' },
						{ label: 'Conditional Compilation', slug: 'language/conditional-compilation' },
						{
							label: 'Attributes',
							items: [
								{ label: 'Overview', slug: 'language/attributes/overview' },
								{ label: '#test', slug: 'language/attributes/test' },
								{ label: '#address', slug: 'language/attributes/address' },
								{ label: '#pure / #reads / #writes', slug: 'language/attributes/pure-reads-writes' },
								{ label: '#ghost', slug: 'language/attributes/ghost' },
								{ label: '#deprecated', slug: 'language/attributes/deprecated' },
							],
						},
						{ label: 'Literate Sysl', slug: 'language/literate-sysl' },
					],
				},
				{
					label: 'Cookbook',
					items: [
						{ label: 'Overview', slug: 'cookbook' },
						{ label: 'Concatenate Strings', slug: 'cookbook/concat-strings' },
						{ label: 'Parse an Integer', slug: 'cookbook/parse-int' },
						{ label: 'Iterate String Bytes vs Chars', slug: 'cookbook/iterate-string' },
						{ label: 'Build a Slice Incrementally', slug: 'cookbook/build-slice' },
						{ label: 'Iterate with Index and Value', slug: 'cookbook/iterate-with-index' },
						{ label: 'Pre-allocate a Slice', slug: 'cookbook/preallocate-slice' },
						{ label: 'Factory Returning &T', slug: 'cookbook/factory-ref' },
						{ label: 'Pass by Value vs Ref', slug: 'cookbook/pass-by-value-vs-ref' },
						{ label: 'Detach &T to T (.copy())', slug: 'cookbook/copy-from-ref' },
						{ label: 'Generic Container', slug: 'cookbook/generic-container' },
						{ label: 'Bounded Generic Compare', slug: 'cookbook/bounded-generic-compare' },
						{ label: 'Multi-bound Type Parameter', slug: 'cookbook/multi-bound' },
						{ label: 'Result + ? Cascade', slug: 'cookbook/result-cascade' },
						{ label: 'Custom Error Enum', slug: 'cookbook/custom-error' },
						{ label: 'Convert Error Types', slug: 'cookbook/error-conversion' },
						{ label: 'Test Expecting a Panic', slug: 'cookbook/test-panic' },
						{ label: 'Capture Output in a Test', slug: 'cookbook/test-capture-output' },
						{ label: 'Test on Multiple Backends', slug: 'cookbook/test-multi-backend' },
						{ label: 'Call a C Function', slug: 'cookbook/call-c' },
						{ label: 'String to C', slug: 'cookbook/string-to-c' },
						{ label: 'Access errno', slug: 'cookbook/errno' },
					],
				},
				{
					label: 'Design Rationale',
					items: [
						{ label: 'Overview', slug: 'rationale' },
						{ label: 'Three-Mode Memory Model', slug: 'rationale/three-mode-model' },
						{ label: 'No Garbage Collector', slug: 'rationale/no-gc' },
						{ label: 'Seven Backends', slug: 'rationale/seven-backends' },
						{ label: 'Contracts First-Class', slug: 'rationale/contracts-first-class' },
						{ label: 'Structural Interfaces', slug: 'rationale/structural-interfaces' },
						{ label: 'Literate Source Files', slug: 'rationale/literate-source' },
					],
				},
				{
					label: 'Architecture',
					items: [
						{ label: 'Overview', slug: 'architecture/overview' },
						{ label: 'Registers', slug: 'architecture/registers' },
						{ label: 'Instruction Set', slug: 'architecture/instruction-set' },
						{ label: 'Memory Model', slug: 'architecture/memory-model' },
						{ label: 'Exceptions & Interrupts', slug: 'architecture/exceptions' },
					],
				},
				{
					label: 'Devices',
					items: [
						{ label: 'Device Map', slug: 'devices/device-map' },
						{ label: 'Timer', slug: 'devices/timer' },
						{ label: 'Interrupt Controller', slug: 'devices/intc' },
						{ label: 'DMA Controller', slug: 'devices/dma' },
						{ label: 'GPIO', slug: 'devices/gpio' },
						{ label: 'UART & PL011', slug: 'devices/uart' },
						{ label: 'Ramdisk & HardDisk', slug: 'devices/disk' },
						{ label: 'MMU', slug: 'devices/mmu' },
						{ label: 'Multi-Core & IPI', slug: 'devices/smp' },
					],
				},
				{
					label: 'Assembly',
					items: [
						{ label: 'Assembler Guide', slug: 'assembly/guide' },
						{ label: 'Calling Convention', slug: 'assembly/abi' },
						{ label: 'TOF Object Format', slug: 'assembly/tof' },
					],
				},
				{
					label: 'SLIX OS',
					items: [
						{ label: 'Overview', slug: 'slix/overview' },
						{ label: 'Kernel', slug: 'slix/kernel' },
						{ label: 'IPC', slug: 'slix/ipc' },
						{ label: 'Servers', slug: 'slix/servers' },
					],
				},
				{
					label: 'CLI Reference',
					items: [
						{ label: 'Commands', slug: 'cli/commands' },
					],
				},
			],
		}),
	],
});
