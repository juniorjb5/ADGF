import pandas as pd
import numpy as np
import streamlit as st

st.title("Taller 1 — Flujo de datos y calidad (demo)")

tickers = ["AAPL", "MSFT"]
dates = pd.date_range("2024-01-01", periods=60, freq="D")

df = pd.DataFrame({
    "ticker": np.repeat(tickers, len(dates)),
    "date": np.tile(dates, len(tickers)),
    "close": np.round(np.random.uniform(80, 120, len(tickers) * len(dates)), 2),
    "volume": np.random.randint(1_000_000, 5_000_000, len(tickers) * len(dates)),
})

st.subheader("Vista previa")
st.dataframe(df.head(20))

st.subheader("Check: duplicados en (ticker, date)")
dups = df.groupby(["ticker", "date"]).size().reset_index(name="n")
st.dataframe(dups[dups["n"] > 1])

st.subheader("Serie de precios")
pivot = df.pivot_table(index="date", columns="ticker", values="close")
st.line_chart(pivot)
