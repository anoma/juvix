function toggle() {
  const toggleAllButton = document.getElementById('toggle-all-button');
  if (toggleAllButton.classList.contains('opened')) {
    document.getElementById('toggle-button-text').textContent = '▶ Open all';
    toggleAllButton.classList.remove('opened');
    document.body
      .querySelectorAll('details')
      .forEach((detail) => detail.removeAttribute('open'));
  } else {
    document.getElementById('toggle-button-text').textContent = '▼ Close all';
    toggleAllButton.classList.add('opened');
    document.body
      .querySelectorAll('details')
      .forEach((detail) => detail.setAttribute('open', ''));
  }
}
