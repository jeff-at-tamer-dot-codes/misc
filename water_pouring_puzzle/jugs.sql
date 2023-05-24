-- Water pouring puzzle
-- https://storage.googleapis.com/jeff-tamer-codes/2023/loel/jugs/jugs.txt
-- https://en.wikipedia.org/wiki/Water_pouring_puzzle
-- PostgreSQL solution by jeff@tamer.codes
-- 2023-05-24

drop table if exists water_puzzle_jugs cascade;
create table water_puzzle_jugs (
  id serial primary key,
  title text,
  capacity int not null,
  initial_volume int not null
    constraint nonnegative_initial_volume
      check (0 <= initial_volume),
  constraint capacity_is_not_less_than_initial_volume
    check (initial_volume <= capacity),
  target_volume int -- null iff any volume of water is acceptable for this jug.
);

drop type if exists water_puzzle_has_tap cascade;
create type water_puzzle_has_tap as enum ('has_tap', 'NOT has_tap');

drop type if exists water_puzzle_has_sink cascade;
create type water_puzzle_has_sink as enum ('has_sink', 'NOT has_sink');

drop type if exists _water_puzzle_node cascade;
create type _water_puzzle_node as (jug_volumes int[]);

drop type if exists _water_puzzle_edge cascade;
create type _water_puzzle_edge as (
  parent_node_index int,
  volume_to_pour int,
  from_jug_index int,
  to_jug_index int,
  child_node_index int
);

-- This function reads the `water_puzzle_jugs` table as input.
drop function if exists water_puzzle_solution(
  water_puzzle_has_tap,
  water_puzzle_has_sink
);
create function water_puzzle_solution(
  has_tap water_puzzle_has_tap,
  has_sink water_puzzle_has_sink
) returns table (
  step int,
  volume_to_pour int,
  from_jug_id int,
  from_jug_title text,
  to_jug_id int,
  to_jug_title text,
  jug_volumes int[]
) as $$
  with recursive
  jugs as (
    select
      id,
      capacity,
      initial_volume,
      target_volume,
      (row_number() over (order by id)) as jug_index
    from water_puzzle_jugs
  ),
  steps as (
    select
      0 as step,
      array[row(
        null, null, null, null,
        1 -- child_node_index
      )::_water_puzzle_edge] as edges,
      array[row(
        array_agg(initial_volume order by id)
      )::_water_puzzle_node] as nodes
    from jugs
  union all
    (with
      previous_step as (select * from steps),
      indices as (
        select (edge).child_node_index as node_index
        from
          previous_step,
          unnest(edges) as edge
      ),
      frontier as (
        select
          node_index,
          (nodes[node_index]).jug_volumes
        from
          indices,
          previous_step
      ),
      node_jug_precheck as (
        select
          jugs.*,
          node_index,
          jug_volumes[jug_index] as volume
        from
          frontier,
          jugs
      ),
      node_jug as (
        select *
        from node_jug_precheck
        where not exists (
          select node_index
          from node_jug_precheck
          group by 1
          having sum((target_volume != volume)::int) = 0
        )
      ),
      taps as (
        select
          node_index,
          capacity - volume as volume_to_pour,
          null::int as from_jug_index,
          null::int as new_from_jug_volume,
          jug_index as to_jug_index,
          capacity as new_to_jug_volume
        from node_jug
        where volume < capacity
          and has_tap = 'has_tap'
      ),
      sinks as (
        select
          node_index,
          volume as volume_to_pour,
          jug_index as from_jug_index,
          0 as new_from_jug_volume,
          null::int as to_jug_index,
          null::int as new_to_jug_volume
        from node_jug
        where 0 < volume
          and has_sink = 'has_sink'
      ),
      before_pouring as (
        select
          node_index,
          (from_jug).jug_index as from_jug_index,
          (from_jug).volume as from_jug_volume,
          (to_jug).jug_index as to_jug_index,
          (to_jug).volume as to_jug_volume,
          least(
            (from_jug).volume,
            (to_jug).capacity - (to_jug).volume
          ) as volume_to_pour
        from node_jug as from_jug
        join node_jug as to_jug using (node_index)
        where
          (from_jug).id != (to_jug).id
          and 0 < (from_jug).volume
          and (to_jug).volume < (to_jug).capacity
      ),
      after_pouring as (
        select
          node_index,
          volume_to_pour,
          from_jug_index,
          from_jug_volume - volume_to_pour
            as new_from_jug_volume,
          to_jug_index,
          to_jug_volume + volume_to_pour
            as new_to_jug_volume
        from before_pouring
        union select * from taps
        union select * from sinks
      ),
      computed_volumes as (
        select
          (node_jug).node_index,
          volume_to_pour,
          from_jug_index,
          to_jug_index,
          jug_index,
          case
            when jug_index = from_jug_index
              then new_from_jug_volume
            when jug_index = to_jug_index
              then new_to_jug_volume
            else
              volume
          end as new_jug_volume
        from node_jug
        join after_pouring using (node_index)
      ),
      computed_edges as (
        select
          node_index as parent_node_index,
          volume_to_pour,
          from_jug_index,
          to_jug_index,
          row(
            array_agg(new_jug_volume order by jug_index)
          )::_water_puzzle_node as child_node
        from computed_volumes
        group by 1, 2, 3, 4
      ),
      filtered_edges as (
        select distinct on (child_node)
          computed_edges.*
        from
          computed_edges,
          previous_step
        where not (child_node = any(nodes))
        order by child_node, random()
      ),
      ordered_edges as (
        select
          filtered_edges.*,
          array_length(nodes, 1) + row_number() over (
          ) as child_node_index
        from
          filtered_edges,
          previous_step
      ),
      new_edge_array as (
        select
          array_agg(row(
            parent_node_index,
            volume_to_pour,
            from_jug_index,
            to_jug_index,
            child_node_index
          )::_water_puzzle_edge) as new_edges
        from ordered_edges
      ),
      new_node_array as (
        select
          nodes || array_agg(
            child_node order by child_node_index
          ) as new_nodes
        from
          ordered_edges,
          previous_step
        group by nodes
      )
      select
        step + 1 as step,
        new_edges as edges,
        new_nodes as nodes
      from
        new_edge_array,
        new_node_array,
        previous_step
    )
  ),
  all_nodes as (select nodes from steps order by step desc limit 1),
  edges as (
    select
      step,
      (edge).*,
      ((all_nodes).nodes[child_node_index]).jug_volumes
    from
      all_nodes,
      steps,
      unnest(edges) as edge
  ),
  final_nodes as (
    select child_node_index
    from edges, jugs
    group by 1
    having sum((target_volume != jug_volumes[jug_index])::int) = 0
  ),
  final_node as (
    select *
    from final_nodes
    order by random()
    limit 1
  ),
  shortest_path as (
    select edges.*
    from edges
    join final_node using (child_node_index)
  union all
    select edges.*
    from
      edges,
      shortest_path
    where
      (edges).child_node_index = (shortest_path).parent_node_index
  )
  select
    step,
    volume_to_pour,
    (from_jugs).id,
    (from_jugs_original).title,
    (to_jugs).id,
    (to_jugs_original).title,
    jug_volumes
  from shortest_path
  left join jugs as from_jugs
    on from_jug_index = (from_jugs).jug_index
  left join water_puzzle_jugs as from_jugs_original
    on (from_jugs).id = (from_jugs_original).id
  left join jugs as to_jugs
    on to_jug_index = (to_jugs).jug_index
  left join water_puzzle_jugs as to_jugs_original
    on (to_jugs).id = (to_jugs_original).id
  order by step
$$ language sql;

-- Solution to the first example given on Wikipedia
delete from water_puzzle_jugs;
insert into water_puzzle_jugs (
  title, capacity, initial_volume, target_volume
) values
  ('the first jug', 8, 8, 4),
  ('the second jug', 5, 0, 4),
  ('the third jug', 3, 0, 0);
select * from water_puzzle_jugs;
select * from water_puzzle_solution(
  'NOT has_tap'::water_puzzle_has_tap,
  'NOT has_sink'::water_puzzle_has_sink
);

-- Solution to the problem statement from Touch
delete from water_puzzle_jugs;
insert into water_puzzle_jugs (
  title, capacity, initial_volume, target_volume
) values
  ('the little jug', 3, 0, null),
  ('the big jug', 5, 0, 4);
select * from water_puzzle_jugs;
select * from water_puzzle_solution(
  'has_tap'::water_puzzle_has_tap,
  'has_sink'::water_puzzle_has_sink
);
