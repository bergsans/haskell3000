import React, { useCallback, useEffect, useRef } from "react";
import { LevelElement, ImagePosition } from "./interfaces";
import { getMousePosition, drawLevel } from "./helpers";
import { TILE_SIZE } from "./constants";

function changeLevel(
  map: string[][],
  pos: ImagePosition,
  imagePos: ImagePosition,
  selectedTileType: string,
  TILE_SIZE: number
): string[][] {
  let newMap = [...map];
  const xPos: number = Math.floor(pos.x / TILE_SIZE);
  newMap[imagePos.y][imagePos.x + xPos] = selectedTileType;
  return newMap;
}

const Level = ({
  isLoadingAssets,
  currentTile,
  types,
  pos,
  level,
  modifyLevel,
  graphics,
}: LevelElement) => {
  const canvasLevelRef = useRef<HTMLCanvasElement>(null);

  const getCoords = useCallback(
    (event: MouseEvent): ImagePosition | undefined => {
      if (!canvasLevelRef.current) {
        return;
      }
      const canvasLevel: HTMLCanvasElement = canvasLevelRef.current;
      return getMousePosition(event, canvasLevel, TILE_SIZE);
    },
    [canvasLevelRef]
  );

  const plotNewTile = useCallback(
    (event: MouseEvent) => {
      const imagePosition: ImagePosition | undefined = getCoords(event);
      if (imagePosition) {
        modifyLevel(
          changeLevel(level, pos, imagePosition, currentTile.type, TILE_SIZE)
        );
      }
    },
    [modifyLevel, pos, getCoords, currentTile, level]
  );

  useEffect(() => {
    if (canvasLevelRef.current) {
      const canvasLevel: HTMLCanvasElement = canvasLevelRef.current;
      const contextLevel = canvasLevel.getContext("2d");
      if (contextLevel && !isLoadingAssets) {
        drawLevel(graphics, contextLevel, types, pos, level);
      }
      canvasLevel.addEventListener("click", plotNewTile);
      return () => canvasLevel.removeEventListener("click", plotNewTile);
    }
  });

  return (
    <canvas className="canvas" width={1024} height={768} ref={canvasLevelRef} />
  );
};
export default Level;
