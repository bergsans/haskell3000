import React from "react";

import LevelEditor from "./level-editor";

import "./app.css";

const App: React.FC = () => {
  return (
    <>
      <div className="container">
        <div className="app">
          <LevelEditor />
        </div>
      </div>
      <div className="only-desktop-error">
        <h1>Only large screen desktop</h1>
      </div>
    </>
  );
};

export default App;
