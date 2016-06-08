## The contents of this file are subject to the Mozilla Public License
## Version 1.1 (the "License"); you may not use this file except in
## compliance with the License. You may obtain a copy of the License
## at http://www.mozilla.org/MPL/
##
## Software distributed under the License is distributed on an "AS IS"
## basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
## the License for the specific language governing rights and
## limitations under the License.
##
## The Original Code is RabbitMQ.
##
## The Initial Developer of the Original Code is GoPivotal, Inc.
## Copyright (c) 2007-2016 Pivotal Software, Inc.  All rights reserved.


defmodule WaitCommandTest do
  use ExUnit.Case, async: false
  import TestHelper

  @command WaitCommand

  setup_all do
    RabbitMQ.CLI.Distribution.start()
    :net_kernel.connect_node(get_rabbit_hostname)

    on_exit([], fn ->
      :erlang.disconnect_node(get_rabbit_hostname)
      :net_kernel.stop()
    end)

    :ok
  end

  setup do
    {:ok, opts: %{node: get_rabbit_hostname}}
  end

  test "validate: with extra arguments returns an arg count error", context do
    assert @command.validate(["pid_file", "extra"], context[:opts]) == {:validation_failure, :too_many_args}
    assert @command.validate([], context[:opts]) == {:validation_failure, :not_enough_args}
  end

  test "banner", context do
    assert @command.banner([], context[:opts]) =~ ~r/Waiting for node #{get_rabbit_hostname}/
  end
end
