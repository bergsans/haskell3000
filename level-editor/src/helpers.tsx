import { ImagePosition, Vector2dLevel } from "./interfaces";
import graphicsSource from "./assets/ap1b_tileset_compact2.png";

export function getGraphics() {
  const image = new Image();
  image.src = graphicsSource;
  return image;
}

export function getMousePosition(
  event: MouseEvent,
  element: HTMLCanvasElement,
  tileSize: number
): ImagePosition {
  const position: ClientRect = element.getBoundingClientRect();
  const x: number = Math.abs(
    Math.round((event.clientX - position.left) / tileSize - 1)
  );
  const y: number = Math.abs(
    Math.round((event.clientY - position.top) / tileSize - 1)
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
  tileSize: number
): ImagePosition[][] {
  return Array.from({ length: h / tileSize }, (_, i) =>
    Array.from({ length: w / tileSize }, (__, j) => ({
      x: j * tileSize,
      y: i * tileSize,
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
  tileSize: number = 48
): string[][] {
  return Array.from({ length: h / tileSize }, (_) =>
    Array.from({ length: w / tileSize }, (__) => "13")
  );
}

export function drawLevel(
  graphics: CanvasImageSource,
  context: CanvasRenderingContext2D,
  types: object,
  pos: Vector2dLevel,
  map: string[][],
  tileSize = 48
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
        tileSize,
        tileSize,
        j * tileSize - pos.x > 0 ? j * tileSize - pos.x : 0,
        i * tileSize - pos.y > 0 ? i * tileSize - pos.y : 0,
        tileSize,
        tileSize
      )
    )
  );
}

export function saveLevelToFile(level) {
  const a = document.createElement("a");
  const asText = level
    .map((ln) => ln.map((el) => parseInt(el, 10)).join(","))
    .join("\n");
  const file = new Blob([asText], { type: "text/json" });
  a.href = URL.createObjectURL(file);
  a.download = "x.level";
  a.click();
}

export function getSheet(sheet, direction, spriteCounter): ImagePosition {
  return sheet[direction][spriteCounter];
}
