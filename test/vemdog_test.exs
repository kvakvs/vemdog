defmodule VemdogTest do
  use ExUnit.Case
  doctest Vemdog

  test "greets the world" do
    assert Vemdog.hello() == :world
  end
end
