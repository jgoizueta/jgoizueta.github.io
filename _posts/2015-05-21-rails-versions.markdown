---
layout: post
title:  "Ruby on Rails Major Releases Cheatsheet"
date:   2014-05-21 21:37:00
categories: frameworks
---

| **Rails 1.0**
| 2005-12
||  *Ruby 1.8.6*
|
| **Rails 1.2**
| 2007-01
||  REST routes
||  respond_to/format
||  `ActiveSupport::Multibyte`
|
| **Rails 2**
| 2007-12
||  RESTful routes improvements; nested routes
||    /people/1/edit instead of /people/1;edit
||  .html.erb instead of .rhtml
||  t.type :name instead of t.column :name, :type
|
| **Rails 2.1**
| 2008-05
||  Time zones
||  Gem dependencies (config.gem in environment.rb)
||  named scopes
||  record changes
||  UTC-named migrations
|
| **Rails 2.2**
| 2008-11
||  *Ruby 1.8.6, 1.8.7*
||  Internationalization framework
||  Thread safety
||  transactional migrations
||  vendorized gems
||  `String#chars` -> `mb_chars`
|
| **Rails 2.3**
| 2009-03
||  *Ruby 1.8.6, 1.8.7, 1.9.1*
||  Engines
||  use Rack middleware, Metal
||  Nested forms
||  dynamic & default scopes
|
| **Rails 3.0*
| 2010-08
||  *Ruby 1.8.6, 1.9.2, 1.9.3*
||  Merb integration
||  ARel query engine
||  new router
||  new action mailer
||  Bundler
||  supports jQuery (not only prototype); Data Mapper (not only ActiveRecord) etc
|
| **Rails 3.1**
| 2011-08
||  asset pipeline
||  HTTP streaming
||  jQuery as default
||  reversible migrations
||  mountable engines
||  mass-assignment protection
|
| **Rails 3.2**
| 2012-01
||  plugins deprecated
||  faster dev mode
||  explain queries
|
| **Rails 4.0**
| 2013-06
||  *Ruby 1.9.3, 2.0*
||  strong parameters
||  Turbolinks
||  live streaming
||  save for threaded servers
|
| **Rails 4.1**
| 2014-04
|| *Ruby 1.9.3, 2.0, 2.1*
||  Spring preloader
||  Variant templates
||  Enums
||  Mailer previews
||  config/secrets.yml
|
| **Rails 4.2**
| 2014-12
||  *Ruby 1.9.3, 2.0, 2.1,  2.2*
||  Active Job framework
||  Adequate Record optimizations
||  Web Console
||  Foreign key support in migration DSL
|
| **Rails 5**
| 2015-...
||  *Ruby 2.2.1*
||  rails command execute rake tasks too
||    rake test TEST=... -> rails test ...
||  rake restart
||  Rails API
||  Turbolinks 3.0
||  Action Cable (WebSockets; uses Redis)
||  Model.where(...).or(Model.where(...))
||  `belongs_to` required by default (=> optional: true)
||  `has_secure_token`
||  `alias_method_chain` deprecated (-> prepend)
