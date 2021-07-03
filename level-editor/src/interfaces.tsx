export interface ImagePosition {
  x: number;
  y: number;
}

export interface LevelSize {
  readonly w: number;
  readonly h: number;
}

export interface ImageSize {
  readonly w: number;
  readonly h: number;
}

export interface Vector2dLevel {
  x: number;
  y: number;
}

export type Level = string[][];

export interface TileSelected {
  x: number;
  y: number;
  type: string;
}

export type ChooseTileFn = (a: TileSelected) => void;

export type ModifyLevelFn = (a: Level) => void;

export interface TilesElement {
  isLoadingAssets: boolean;
  currentTile: TileSelected;
  tiles: ImagePosition[][];
  types: object;
  chooseTile: ChooseTileFn;
  graphics: HTMLImageElement;
}

export interface LevelElement {
  isLoadingAssets: boolean;
  currentTile: TileSelected;
  types: object;
  pos: ImagePosition;
  level: string[][];
  modifyLevel: ModifyLevelFn;
  graphics: HTMLImageElement;
}
