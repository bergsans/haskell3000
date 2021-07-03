import React, { useState, useEffect } from "react";

import Tiles from "./level-editor-display-tiles";
import Level from "./level-editor-display-level";
import {
  ModifyLevelFn,
  TileSelected,
  ChooseTileFn,
  Vector2dLevel,
  ImagePosition,
} from "./interfaces";
import {
  saveLevelToFile,
  initLevel,
  getTypes,
  getTiles,
  getGraphics,
} from "./helpers";
import { tileSize } from "./constants";

import arrowLeft from "./assets/arrow-left.png";
import arrowRight from "./assets/arrow-right.png";

function updateLevelView(
  pos: Vector2dLevel,
  keyboardAction: string | null = null,
  tileSize: number = 48,
  levelWidth: number = 48
): Vector2dLevel {
  if (keyboardAction === "ArrowRight") {
    pos.x =
      pos.x + tileSize <= levelWidth * tileSize ? (pos.x += tileSize) : pos.x;
  } else if (keyboardAction === "ArrowLeft") {
    pos.x = pos.x - tileSize >= 0 ? (pos.x -= tileSize) : pos.x;
  }
  return { x: pos.x, y: pos.y };
}

const LevelEditor = () => {
  const graphics = getGraphics();
  const tiles: ImagePosition[][] = getTiles(768, 384, tileSize);
  const types: object = getTypes(tiles);

  const [isLoadingAssets, setIsLoadingAssets] = useState<boolean>(true);
  const [fileName, setFilename] = useState<string>("");
  const [selectedTile, setSelectedTile] = useState<TileSelected>({
    x: 4,
    y: 0,
    type: "4",
  });
  const [level, setLevel] = useState(initLevel());
  const [pos, setPos] = useState({ x: 0, y: 0 });

  const moveLevelArrowKeys = (event: KeyboardEvent) =>
    setPos(updateLevelView(pos, event.key));
  const moveLevelByArrowClick = (direction: string) =>
    setPos(updateLevelView(pos, direction));
  const chooseTile: ChooseTileFn = (tile) => setSelectedTile(tile);
  const modifyLevel: ModifyLevelFn = (level) => setLevel(level);
  const saveLevel = () => saveLevelToFile(level, fileName);
  const resetLevel = () => modifyLevel(initLevel());
  useEffect(() => {
    graphics.onload = () => setIsLoadingAssets(false);
  });

  useEffect(() => {
    document.addEventListener("keydown", moveLevelArrowKeys);
    return () => document.removeEventListener("keydown", moveLevelArrowKeys);
  });

  return (
    <div className="editor">
      <div className="level">
        <Level
          isLoadingAssets={isLoadingAssets}
          currentTile={selectedTile}
          types={types}
          pos={pos}
          level={level}
          modifyLevel={modifyLevel}
          graphics={graphics}
        />
        <img
          src={arrowLeft}
          className="left-arrow"
          title="move level left"
          onClick={() => moveLevelByArrowClick("ArrowLeft")}
          alt=""
        />
        <img
          src={arrowRight}
          className="right-arrow"
          title="move level right"
          alt=""
          onClick={() => moveLevelByArrowClick("ArrowRight")}
        />
      </div>
      <div className="tiles">
        <Tiles
          isLoadingAssets={isLoadingAssets}
          currentTile={selectedTile}
          tiles={tiles}
          types={types}
          chooseTile={chooseTile}
          graphics={graphics}
        />
        <p>
          <input
            value={fileName}
            placeholder="file-name.level"
            onChange={(event) => setFilename(event.target.value)}
          />
          <button onClick={saveLevel}>SAVE LEVEL</button>
          <span className="separator"> | </span>
          <button onClick={resetLevel}>RESET LEVEL</button>
        </p>
      </div>
    </div>
  );
};
export default LevelEditor;
