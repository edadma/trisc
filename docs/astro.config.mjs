// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

export default defineConfig({
	site: 'https://trisc.dev',
	integrations: [
		starlight({
			title: 'TRISC',
			social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/edadma/trisc' }],
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
