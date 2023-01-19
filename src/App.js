import "./App.css";
import linkData from "./Api";

function App() {
  return (
    <div className="App">
      <div className="wrapper">
        <ol>
          {linkData.map(({ id, text, href }) => (
            <li key={id}>
              <a href={href} target="_blank" rel="noreferrer">
                {text}
              </a>
            </li>
          ))}
        </ol>
      </div>
    </div>
  );
}

export default App;