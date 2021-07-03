import React, { useCallback, useEffect, useRef } from "react";
import { getMousePosition, clear } from "./helpers";
import { tileSize } from "./constants";
import { TilesElement, TileSelected, ImagePosition } from "./interfaces";

function selectTileType(
  selectedTile: TileSelected,
  position: ImagePosition,
  types: object,
  tileSize: number = 48
): TileSelected {
  const typeIndex: number = Object.values(types).findIndex(
    (tile) =>
      tile.x / tileSize === position.x && tile.y / tileSize === position.y
  );
  if (typeIndex >= 0) {
    return {
      x: types[typeIndex].x / tileSize,
      y: types[typeIndex].y / tileSize,
      type: typeIndex.toString(),
    };
  }
  return selectedTile;
}

function drawTilesUI(
  graphics: HTMLImageElement,
  context: CanvasRenderingContext2D,
  selectedTile: TileSelected,
  tiles: Array<{ x: number; y: number }[]>,
  tileSize: number = 48
) {
  tiles.forEach((row, i) =>
    row.forEach((tileType, j) => {
      context.drawImage(
        graphics,
        tileType.x,
        tileType.y,
        tileSize,
        tileSize,
        tileType.x + (j % 5) + 1,
        tileType.y + (i % 11) + 1,
        tileSize,
        tileSize
      );
      if (i === selectedTile.y && j === selectedTile.x) {
        context.lineWidth = 2;
        context.strokeStyle = "red";
        const x: number = tileType.x + (j % 5);
        const y: number = tileType.y + (i % 11);
        context.strokeRect(x, y, tileSize, tileSize);
      }
    })
  );
}

const Tiles = ({
  isLoadingAssets,
  currentTile,
  tiles,
  types,
  chooseTile,
  graphics,
}: TilesElement) => {
  const canvasTilesRef = useRef<HTMLCanvasElement>(null);

  const getCoords = useCallback(
    (event: MouseEvent): ImagePosition => {
      if (!canvasTilesRef.current) {
        return { x: currentTile.x, y: currentTile.y };
      }
      const canvasTiles: HTMLCanvasElement = canvasTilesRef.current;
      return getMousePosition(event, canvasTiles, tileSize);
    },
    [canvasTilesRef, currentTile]
  );

  const selectNewTile = useCallback(
    (event: MouseEvent) => {
      const imagePosition: ImagePosition = getCoords(event);
      chooseTile(selectTileType(currentTile, imagePosition, types));
    },
    [chooseTile, getCoords, currentTile, types]
  );

  useEffect(() => {
    if (canvasTilesRef.current) {
      const canvasTiles: HTMLCanvasElement = canvasTilesRef.current;
      const contextTiles = canvasTiles.getContext("2d");
      if (contextTiles && !isLoadingAssets) {
        clear(contextTiles, 768, 393);
        drawTilesUI(graphics, contextTiles, currentTile, tiles);
      }
      canvasTiles.addEventListener("click", selectNewTile);
      return () => canvasTiles.removeEventListener("click", selectNewTile);
    }
  });

  return <canvas ref={canvasTilesRef} width={768} height={393} />;
};
export default Tiles;
