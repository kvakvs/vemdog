defmodule Vemdog do
  @moduledoc """
  Starts collection of trace data, holds the trace data and serves a web page 
  to explore the diagrams.
  """

  @doc """
  Start tracing all new processes from this moment. Will also expand tracing to 
  existing processes which establish link/monitor relations with the traced ones

  ## Examples

      iex> Vemdog.new()
      :ok
  """
  def new do
    :world
  end
end
