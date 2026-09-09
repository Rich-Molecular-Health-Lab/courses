```{=html}
<ul class="list">
<% for (const item of items) { %>
  <li <%= metadataAttrs(item) %>>
    <span class="badge bg-secondary listing-category"><%= item.category %></span>
    <a href="<%- item.href %>" class="listing-title"><%= item.title %></a>
  </li>
<% } %>
</ul>
```
