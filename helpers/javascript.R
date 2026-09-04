merge_rows <- function() {
  JS("
    function(rowInfo, column, state) {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.values[column.id] === prevRow[column.id]) {
            return { visibility: 'hidden' }
          }
      }
  ")
}

special_style <- function() {
  JS("
    function(cellInfo) {
    if (cellInfo.value && cellInfo.value.includes('Case Conversation')) {
      return { backgroundColor: '#18bc9c', color: '#fff', fontWeight: 'bold', fontVariant: 'small-caps', border: '1px outset #fff' };
    } else if (cellInfo.value && cellInfo.value.includes('Film')) {
      return { backgroundColor: '#3498db', color: '#fff', fontWeight: 'bold', fontVariant: 'small-caps', border: '1px outset #fff' };
    }
    return ;
    }
     ")
}

special_class <- function() {
  JS("
    function(rowInfo) {
    if (rowInfo.values['special'].includes('Case Conversation')) {
      return 'table-success';
    } else if (rowInfo.values['special'].includes('Film')) {
      return 'table-info';
    }
    }
     ")
}

topic_class <- function() {
  JS("
    function(rowInfo) {
    if (rowInfo.values['row'] === 'dexam') {
      return 'exam';
    } else {
      return ;
    }
    }
     ")
}

agenda_detail <- function() {
  JS("
  function(rowInfo) {
    if (rowInfo.values['special'] && rowInfo.values['special'].trim().length > 0) {
    return `${rowInfo.values['detail']}`;
    }
  }
     ")
}