import { Level, ImagePosition, Vector2dLevel } from "./interfaces";
import graphicsSource from "./assets/ap1b_tileset_compact2.png";

export function getGraphics() {
  const image = new Image();
  image.src = graphicsSource;
  return image;
}

export function getMousePosition(
  event: MouseEvent,
  element: HTMLCanvasElement,
  TILE_SIZE: number
): ImagePosition {
  const position: ClientRect = element.getBoundingClientRect();
  const x: number = Math.abs(
    Math.round((event.clientX - position.left) / TILE_SIZE - 1)
  );
  const y: number = Math.abs(
    Math.round((event.clientY - position.top) / TILE_SIZE - 1)
  );
  return { x, y };
}

export function clear(
  context: CanvasRenderingContext2D,
  width: number,
  height: number
): void {
  context.fillStyle = "#8fbcbb";
  context.fillRect(0, 0, width, height);
}

export function getTiles(
  w: number,
  h: number,
  TILE_SIZE: number
): ImagePosition[][] {
  return Array.from({ length: h / TILE_SIZE }, (_, i) =>
    Array.from({ length: w / TILE_SIZE }, (__, j) => ({
      x: j * TILE_SIZE,
      y: i * TILE_SIZE,
    }))
  );
}

export function getTypes(tiles: ImagePosition[][]): object {
  return tiles
    .flat()
    .reduce((allTypes, type, i) => ({ ...allTypes, [i]: type }), {});
}

export function initLevel(
  w: number = 1024 * 2,
  h: number = 768,
  TILE_SIZE: number = 48
): Level {
  return Array.from({ length: h / TILE_SIZE }, (_) =>
    Array.from({ length: w / TILE_SIZE }, (__) => "13")
  );
}

export function drawLevel(
  graphics: CanvasImageSource,
  context: CanvasRenderingContext2D,
  types: object,
  pos: Vector2dLevel,
  map: string[][],
  TILE_SIZE = 48
) {
  if (!map) {
    return;
  }
  map.forEach((row: string[], i: number) =>
    row.forEach((tile: string, j: number) =>
      context.drawImage(
        graphics,
        types[tile].x,
        types[tile].y,
        TILE_SIZE,
        TILE_SIZE,
        j * TILE_SIZE - pos.x > 0 ? j * TILE_SIZE - pos.x : 0,
        i * TILE_SIZE - pos.y > 0 ? i * TILE_SIZE - pos.y : 0,
        TILE_SIZE,
        TILE_SIZE
      )
    )
  );
}

export function saveLevelToFile(level: Level, fileName: string) {
  const a = document.createElement("a");
  const asText = level
    .map((ln) => ln.map((el) => parseInt(el, 10)).join(","))
    .join("\n");
  const file = new Blob([asText], { type: "text/json" });
  a.href = URL.createObjectURL(file);
  a.download = fileName;
  a.click();
}

export function getSheet(
  sheet: ImagePosition,
  direction: keyof ImagePosition,
  spriteCounter: number
): ImagePosition {
  return sheet[direction][spriteCounter];
}
