/**
 * Shapes for everything under `content/`.
 *
 * These types are the contract between the JSON files and the UI: if a JSON file drifts from its
 * shape here, `src/__tests__/content.test.ts` fails the build.
 */

/* ---------- Finder filesystem (derived, never written by hand) ---------- */

export type LocationId = 'work' | 'about' | 'resume' | 'archive';

export type FileType = 'txt' | 'img' | 'url' | 'fig' | 'pdf';

export interface FileNode {
	id: string;
	name: string;
	icon: string;
	kind: 'file';
	fileType: FileType;
	position?: string;
	href?: string;
	imageUrl?: string;
	subtitle?: string;
	description?: string[];
}

export interface FolderNode {
	id: string;
	name: string;
	icon: string;
	kind: 'folder';
	position?: string;
	windowPosition?: string;
	children: TreeNode[];
}

export type TreeNode = FolderNode | FileNode;

export interface Location {
	name: string;
	icon: string;
	kind: 'folder';
	children: TreeNode[];
}

/* ---------- Shell files ---------- */

export interface SiteConfig {
	name: string;
	brand: string;
	url: string;
	/** `{name}` is replaced with `name`. */
	title: string;
	description: string;
	notFound: { title: string; description: string };
	themeColor: string;
	theme: { default: 'light' | 'dark' };
	favicon: string;
	ui: {
		emptyFolder: string;
		noFileSelected: string;
		noImageSelected: string;
		notFoundHeading: string;
		notFoundBody: string;
		notFoundBack: string;
	};
}

export interface ProfileConfig {
	name: string;
	greeting: string;
	headline: string;
	avatar?: string;
	about?: {
		enabled?: boolean;
		location: { name: string; icon: string };
		title?: string;
		lines: string[];
		photos?: { name: string; src: string; position?: string }[];
	};
}

export interface MenuConfig {
	links: { label: string; section: SectionId }[];
	statusIcons: { icon: string; label: string }[];
	appearanceToggle: { icon: string; label: string };
	closeAllLabel: string;
	dock: SectionId[];
}

export interface DesktopMenuItem {
	label?: string;
	hint?: string;
	disabled?: boolean;
	separator?: boolean;
	action?: 'open' | 'info' | 'quit';
	section?: SectionId;
}

export interface DesktopConfig {
	contextMenu: DesktopMenuItem[];
	/** Menu for a desktop shortcut. */
	iconContextMenu: DesktopMenuItem[];
	/** Menu for a dock icon. */
	dockContextMenu: DesktopMenuItem[];
	info: { name: string; subtitle?: string; lines: string[] };
}

/**
 * A phone tile, dock icon or tab. `section` is optional: the design's home grid includes iOS system
 * apps that have no content behind them, so leaving it out makes the tile decorative.
 */
export interface PhoneApp {
	label: string;
	icon: string;
	section?: SectionId;
	/** Finder location to open when the section is the finder. */
	location?: LocationId;
}

export interface PhoneConfig {
	wallpaper: string;
	search: { placeholder: string };
	grid: PhoneApp[];
	dock: PhoneApp[];
	tabs: PhoneApp[];
}

/* ---------- Sections ---------- */

export type SectionId =
	| 'projects'
	| 'articles'
	| 'gallery'
	| 'contact'
	| 'skills'
	| 'resume'
	| 'archive';

/** Every section file starts with these. */
export interface SectionBase {
	enabled?: boolean;
	/** Window title. */
	title: string;
	/** Dock label; defaults to `title` when omitted. */
	name?: string;
	/** Dock/app icon path, e.g. `/images/finder.png`. */
	icon: string;
}

export interface ProjectsSection extends SectionBase {
	location: { name: string; icon: string };
	sidebarHeading?: string;
	empty?: string;
	projects: {
		id: string;
		name: string;
		description?: string[];
		url?: string;
		image?: string;
		figma?: string;
		folderPosition?: string;
		windowPosition?: string;
		desktop?: { enabled?: boolean; position?: string };
	}[];
}

export interface ArticlesSection extends SectionBase {
	url?: string;
	heading?: string;
	headingColor?: string;
	cta?: string;
	posts: { date: string; title: string; image?: string; link: string }[];
}

export interface GallerySection extends SectionBase {
	sidebarHeading?: string;
	albums: { title: string; icon: string }[];
	photos: { src: string; alt?: string }[];
}

export interface ContactSection extends SectionBase {
	heading?: string;
	blurb?: string;
	avatar?: string;
	links: { text: string; icon: string; bg?: string; link: string }[];
}

export interface SkillsSection extends SectionBase {
	prompt?: string;
	command?: string;
	columns?: [string, string];
	groups: { category: string; items: string[] }[];
	footnote?: { loaded?: string; renderTime?: string };
}

export interface ResumeSection extends SectionBase {
	location: { name: string; icon: string };
	fileName?: string;
	file: string;
	loading?: string;
	error?: string;
	openLabel?: string;
}

export interface ArchiveSection extends SectionBase {
	location: { name: string; icon: string };
	items: { name: string; src: string; position?: string }[];
}

/** Every section file, keyed by its filename (minus `.json`). */
export interface SectionData {
	projects: ProjectsSection;
	articles: ArticlesSection;
	gallery: GallerySection;
	contact: ContactSection;
	skills: SkillsSection;
	resume: ResumeSection;
	archive: ArchiveSection;
}

export type SectionFile = Partial<SectionData>;

/* ---------- Derived shell data ---------- */

export interface DesktopShortcut {
	id: string;
	name: string;
	icon: string;
	position: string;
	description?: string[];
}
