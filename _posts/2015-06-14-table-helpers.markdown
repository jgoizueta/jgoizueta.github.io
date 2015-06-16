---
layout: post
title:  "Rails Table Helpers"
date:   2015-06-14 20:36:00
categories: techniques
---

Tables in HTML are defined row-wise.
It is often annoying having to define a column's header separate from
the values.

For example, in a Rails view, if we use some logic to exclude a column we must repeat the logic for the header and the values.

```erb
<table>
  <tr>
    <th>Name</th>
    <% unless no_dates %>
      <th>Date</th>
    <% end %>
  </tr>
  <% for record in @records %>
    <tr>
      <td><%= record.name %></td>
      <% unless no_dates %>
        <td><%= record.date %></td>
      <% end %>
    </tr>
  <% end %>
</table>
```

But, since we're not really writing HTML; we're *generating* it
with Ruby, we can arrange things in a better way. We can
put some lambdas to good use here and take advantace of the
nice Ruby syntax to write our table as:

```erb
<%= table_by_columns(@records) do %>
  <% table_column('Name') { |record| record.name } %>
  <% table_column('Date') { |record| record.date } unless no_dates %>
<% end %>
```

To achieve this convenient syntax we just need a couple of helpers.
We define `table_by_columns` passing a block to it
where `table_column` calls are issued to define the table columns.

To define the column values a block is passed which recieves the record
for which the column value is to be generated.

The data for each column is kept and used by `table_by_columns`
to generate the table after the block executes.

```ruby
module TableByColumnsHelper

  def table_by_columns(records, *cls, &blk)
    raise "Tables cannot be nested" unless @table_listing_columns.nil?
    @table_listing_columns = []
    blk.call
    columns = @table_listing_columns
    @table_listing_columns = nil
    if columns.present?
      content_tag :table, class: cls do
        content_tag :tr do
          columns.each do |header, value_method|
            concat content_tag(:th, header)
          end
        end
        records.each do |record|
          concat(content_tag(:tr) do
            columns.each do |header, value_method|
              concat content_tag(:td, value_method[record])
            end
          end)
        end
      end
    end
  end

  def table_column(header, &blk)
    raise "table_column out of table_by_columns block" if @table_listing_columns.nil?
    @table_listing_columns << [header, blk]
  end

end
```

Ugh, did you notice the odd parentheses around the second `:tr`?

That's a glitch of Ruby's otherwise nice syntax. (wihtout them the `do` would
have been attached to `concat`; we could have used curly braces for the block,
but that's inconsistent with the rest of the code)
We'll do something about it in the next snippets.


Now that we have this nice helpers we can have even more convenience easily.
Say that when consecutive rows repeat the same value for a column we'd prefer
to have a merged single cell for them using `rowspan`.

Instead of:

![Table with repeating values](/assets/table_no_grouping.png)

We might prefer:

![Table with merged cells](/assets/table_grouping.png)


We can solve this with our helpers by adding some option to the columns
we want to merge in that way:

```erb
<%= table_by_columns(@records) do %>
  <% table_column('Name', grouping: true) { |record| record.name } %>
  <% table_column('Date') { |record| record.date } %>
<% end %>
```

The problem to do this is we cannot generate a `td`
until we know how many consecutive rows will have the same value.
We also don't want to read the whole collection and keep data around since
there may be a lot of records.

An easy way to solve this is using this
[look_ahead_iterator gem](https://github.com/jgoizueta/look_ahead_iterator)
that allow us to prefetch values from an enumerable. The `LookAheadIterator`
acts as an iterator wrapper that caches prefetched values. In the worst case
we could end up prefetching the whole collection, but in typical use we
expect to have to cache only a few values.

Here's the code to implement this *cell-merging*.
That `concat_content_tag` method is defined first because
I don't want more odd parentheses!


```ruby
module TableByColumnsHelper

    def concat_content_tag(*args, &blk)
      concat content_tag(*args, &blk)
    end

    def table_by_columns(records, *cls, &blk)
      raise "Tables cannot be nested" unless @table_listing_columns.nil?
      options = cls.extract_options!
      @table_listing_columns = []
      blk.call
      columns = @table_listing_columns
      @table_listing_columns = nil
      nested_grouping = options[:nested_grouping]
      if columns.present?
        column_grouping = columns.any? { |h, v, options, row_count| row_count }
        content_tag :table, class: cls do
          content_tag :tr do
            columns.each do |header, value_method, options, row_count|
              concat_content_tag :th, header
            end
          end
          records = LookAheadIterator.new(records, stop: true) if column_grouping
          records.each do |record|
            concat_content_tag :tr do
              columns.each do |column|
                header, value_method, options, num = column
                value = value_method[record]
                if num.nil?
                  concat_content_tag :td, value
                else
                  # grouping column
                  num -= 1
                  if num == 0
                    loop do
                      num += 1
                      other_record = records.look_ahead(num)
                      other_value = other_record && value_method[other_record]
                      break if !other_value || other_value != value
                    end
                    concat_content_tag :td, value, rowspan: num
                  end
                  column[3] = num
                end
              end
            end
          end
        end
      end
    end

    # Passing the option +grouping: true+ to a column
    # causes its rows to be grouped (with colspan).
    def table_column(header, options = {}, &blk)
      raise "table_column out of table_by_columns block" if @table_listing_columns.nil?
      row_count = options[:grouping] ? 1 : nil
      @table_listing_columns << [header, blk, options, row_count]
    end

  end
```

We had to complicate a bit those helpers, but our views are still nice and clean.

There's still one thing we can improve, say we're using our cell-merging
in two columns:

```erb
<%= table_by_columns(@records) do %>
  <% table_column('Name', grouping: true) { |record| record.name } %>
  <% table_column('Date', grouping: true) { |record| record.date } %>
<% end %>
```

This could result in something like this:

![Table with uneven rows](/assets/table_grouping2a.png)

This may not be what we want. Usually, specially if columns appear in sorting
order we'd prefer mergec cells to run across preceding column's cells:

![Table with uneven rows](/assets/table_grouping2b.png)

We add an option to avoid this case:

```erb
<%= table_by_columns(@records, nested_grouping: true) do %>
  <% table_column('Name', grouping: true) { |record| record.name } %>
  <% table_column('Date', grouping: true) { |record| record.date } %>
<% end %>
```

around fix our helpers to support that option:

```ruby
module TableByColumnsHelper

    def concat_content_tag(*args, &blk)
      concat content_tag(*args, &blk)
    end

    def table_by_columns(records, *cls, &blk)
      raise "Tables cannot be nested" unless @table_listing_columns.nil?
      options = cls.extract_options!
      @table_listing_columns = []
      blk.call
      columns = @table_listing_columns
      @table_listing_columns = nil
      nested_grouping = options[:nested_grouping]
      if columns.present?
        column_grouping = columns.any? { |h, v, options, row_count| row_count }
        content_tag :table, class: cls do
          content_tag :tr do
            columns.each do |header, value_method, options, row_count|
              concat_content_tag :th, header
            end
          end
          base_nested_max_num = nil
          records = LookAheadIterator.new(records, stop: true) if column_grouping
          records.each do |record|
            concat_content_tag :tr do
              nested_max_num = base_nested_max_num
              columns.each do |column|
                header, value_method, options, num = column
                value = value_method[record]
                if num.nil?
                  concat_content_tag :td, value
                else
                  # grouping column
                  num -= 1
                  if num == 0
                    loop do
                      num += 1
                      other_record = records.look_ahead(num)
                      other_value = other_record && value_method[other_record]
                      break if !other_value || other_value != value
                    end
                    num = [num, nested_max_num].min if nested_max_num
                    concat_content_tag :td, value, rowspan: num
                  end
                  column[3] = num
                  nested_max_num = [nested_max_num || num, num].min if nested_grouping
                end
              end
            end
            base_nested_max_num -= 1 if base_nested_max_num
          end
        end
      end
    end

    # Passing the option +grouping: true+ to a column
    # causes its rows to be grouped (with colspan).
    def table_column(header, options = {}, &blk)
      raise "table_column out of table_by_columns block" if @table_listing_columns.nil?
      row_count = options[:grouping] ? 1 : nil
      @table_listing_columns << [header, blk, options, row_count]
    end

  end
```
