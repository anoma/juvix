function toggle() {
  const toggleAllButton = document.getElementById('toggle-all-button');
  if (toggleAllButton.classList.contains('opened')) {
    document.getElementById('toggle-button-text').textContent =
      '▶ Expand all modules';
    toggleAllButton.classList.remove('opened');
    document.body
      .querySelectorAll('details')
      .forEach((detail) => detail.removeAttribute('open'));
  } else {
    document.getElementById('toggle-button-text').textContent =
      '▼   Hide all modules';
    toggleAllButton.classList.add('opened');
    document.body
      .querySelectorAll('details')
      .forEach((detail) => detail.setAttribute('open', ''));
  }
}
