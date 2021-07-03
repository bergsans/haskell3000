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
export interface TileSelected {
  x: number;
  y: number;
  type: string;
}
export interface TilesElement {
  isLoadingAssets: boolean;
  currentTile: TileSelected;
  tiles: ImagePosition[][];
  types: object;
  chooseTile;
  graphics;
}
export interface LevelElement {
  isLoadingAssets: boolean;
  currentTile: TileSelected;
  types: object;
  pos: ImagePosition;
  level: string[][];
  modifyLevel;
  graphics;
}
