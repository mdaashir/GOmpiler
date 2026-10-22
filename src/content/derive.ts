import { icon } from './icons';
import { hasSection, isSectionEnabled, profile, sectionData } from './loader';
import type {
	DesktopShortcut,
	FileNode,
	FolderNode,
	Location,
	LocationId,
	TreeNode,
} from './types';

/** Icons and in-window placement for the file nodes generated from each project. */
const FILE_ICONS = {
	txt: icon('files.txt'),
	url: icon('apps.safari'),
	img: icon('files.image'),
	fig: icon('files.plain'),
	pdf: icon('files.pdf'),
};

const FILE_LAYOUT = ['top-5 left-10', 'top-10 right-20', 'top-52 right-80', 'top-60 right-20'];
const FOLDER_LAYOUT = ['top-10 left-5', 'top-52 right-80', 'top-10 left-80'];
const DESKTOP_LAYOUT = ['top-[5vh] left-5', 'top-[20vh] left-7', 'top-[33vh] left-7'];

const FOLDER_ICON = icon('files.folder');

function file(node: Omit<FileNode, 'kind' | 'position'> & { position?: string }): FileNode {
	return { kind: 'file', ...node };
}

/**
 * A project becomes a folder whose contents are generated from whichever fields are present —
 * no description means no `.txt`, no `figma` means no `Design.fig`.
 */
function projectFolder(
	project: NonNullable<typeof sectionData.projects>['projects'][number],
	index: number
): FolderNode {
	const children: TreeNode[] = [];

	if (project.description?.length) {
		children.push(
			file({
				id: `${project.id}-txt`,
				name: `${project.name}.txt`,
				icon: FILE_ICONS.txt,
				fileType: 'txt',
				description: project.description,
			})
		);
	}
	if (project.url) {
		children.push(
			file({
				id: `${project.id}-url`,
				name: `${project.id}.app`,
				icon: FILE_ICONS.url,
				fileType: 'url',
				href: project.url,
			})
		);
	}
	if (project.image) {
		children.push(
			file({
				id: `${project.id}-img`,
				name: `${project.id}.png`,
				icon: FILE_ICONS.img,
				fileType: 'img',
				imageUrl: project.image,
			})
		);
	}
	if (project.figma) {
		children.push(
			file({
				id: `${project.id}-fig`,
				name: 'Design.fig',
				icon: FILE_ICONS.fig,
				fileType: 'fig',
				href: project.figma,
			})
		);
	}

	children.forEach((child, childIndex) => {
		child.position ??= FILE_LAYOUT[childIndex % FILE_LAYOUT.length];
	});

	return {
		kind: 'folder',
		id: `project-${project.id}`,
		name: project.name,
		icon: FOLDER_ICON,
		position: project.folderPosition ?? FOLDER_LAYOUT[index % FOLDER_LAYOUT.length],
		windowPosition: project.windowPosition,
		children,
	};
}

/**
 * The Finder's sidebar locations, built from whichever sections are available. A section with no
 * content contributes no location, so the sidebar never shows an empty folder.
 */
export function buildLocations(): Partial<Record<LocationId, Location>> {
	const locations: Partial<Record<LocationId, Location>> = {};

	const projects = sectionData.projects;
	if (isSectionEnabled('projects') && projects) {
		locations.work = {
			name: projects.location.name,
			icon: projects.location.icon,
			kind: 'folder',
			children: projects.projects.map(projectFolder),
		};
	}

	const about = profile.about;
	if (about && about.enabled !== false && about.lines.length > 0) {
		const children: TreeNode[] = (about.photos ?? []).map((photo, index) =>
			file({
				id: `about-${photo.name}`,
				name: photo.name,
				icon: FILE_ICONS.img,
				fileType: 'img',
				imageUrl: photo.src,
				position: photo.position ?? FILE_LAYOUT[index % FILE_LAYOUT.length],
			})
		);
		children.push(
			file({
				id: 'about-txt',
				name: 'about-me.txt',
				icon: FILE_ICONS.txt,
				fileType: 'txt',
				subtitle: about.title,
				description: about.lines,
				position: 'top-60 left-5',
			})
		);
		locations.about = {
			name: about.location.name,
			icon: about.location.icon,
			kind: 'folder',
			children,
		};
	}

	const resume = sectionData.resume;
	if (isSectionEnabled('resume') && resume) {
		locations.resume = {
			name: resume.location.name,
			icon: resume.location.icon,
			kind: 'folder',
			children: [
				file({
					id: 'resume-pdf',
					name: resume.fileName ?? 'Resume.pdf',
					icon: FILE_ICONS.pdf,
					fileType: 'pdf',
					position: FOLDER_LAYOUT[0],
				}),
			],
		};
	}

	const archive = sectionData.archive;
	if (isSectionEnabled('archive') && archive) {
		locations.archive = {
			name: archive.location.name,
			icon: archive.location.icon,
			kind: 'folder',
			children: archive.items.map((item, index) =>
				file({
					id: `archive-${item.name}`,
					name: item.name,
					icon: FILE_ICONS.img,
					fileType: 'img',
					imageUrl: item.src,
					position: item.position ?? FILE_LAYOUT[index % FILE_LAYOUT.length],
				})
			),
		};
	}

	return locations;
}

/** Desktop shortcuts are the projects, unless a project opts out with `desktop.enabled: false`. */
export function buildShortcuts(): DesktopShortcut[] {
	const projects = sectionData.projects;
	if (!hasSection('projects') || !projects) return [];

	return projects.projects
		.filter((project) => project.desktop?.enabled !== false)
		.map((project, index) => ({
			id: project.id,
			name: project.name,
			icon: FOLDER_ICON,
			position: project.desktop?.position ?? DESKTOP_LAYOUT[index % DESKTOP_LAYOUT.length],
			description: project.description,
		}));
}

/** The order the Finder sidebar lists its locations in. */
export const LOCATION_ORDER: LocationId[] = ['work', 'about', 'resume', 'archive'];
