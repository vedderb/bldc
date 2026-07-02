/*
  Copyright 2026 Joel Svensson  svenssonjoel@yahoo.se

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "QLbmQuickDisplayItem.h"
#include <QPainter>
#include <QSGSimpleTextureNode>
#include <QQuickWindow>

QLbmQuickDisplayItem::QLbmQuickDisplayItem(int displayWidth, int displayHeight,
                                           QQuickItem *parent)
  : QQuickItem(parent)
  , m_image(displayWidth, displayHeight, QImage::Format_RGB32)
  , m_pending(true)
  , m_displayWidth(displayWidth)
  , m_displayHeight(displayHeight) {
  m_image.fill(Qt::black);
  setImplicitWidth(displayWidth);
  setImplicitHeight(displayHeight);
  setFlag(ItemHasContents, true);
}

void QLbmQuickDisplayItem::setImage(const QImage &img) {
  QMutexLocker lock(&m_mutex);
  m_image = img.scaled(m_displayWidth, m_displayHeight).convertToFormat(QImage::Format_RGB32);
  m_pending = true;
  update();
}

void QLbmQuickDisplayItem::setImageAt(int x, int y, const QImage &img) {
  QMutexLocker lock(&m_mutex);
  QPainter p(&m_image);
  p.drawImage(x, y, img);
  m_pending = true;
  update();
}

// Called on the render thread during the scene-graph sync phase (main thread
// is blocked). Takes a COW snapshot of m_image under the mutex so the lock is
// held only for an O(1) pointer bump, then uploads to a GPU texture outside
// the lock. On real hardware the GPU composites from here; on the emulator
// Swift Shader still uses CPU but with one fewer software-render pass compared
// to the old QQuickPaintedItem path.
QSGNode *QLbmQuickDisplayItem::updatePaintNode(QSGNode *oldNode, UpdatePaintNodeData *) {
  QSGSimpleTextureNode *node = static_cast<QSGSimpleTextureNode *>(oldNode);
  if (!node) {
    node = new QSGSimpleTextureNode();
    node->setFiltering(QSGTexture::Linear);
    node->setOwnsTexture(true);
  }

  QImage snapshot;
  {
    QMutexLocker lock(&m_mutex);
    if (m_pending) {
      snapshot = m_image;  // COW: O(1) reference-count bump
      m_pending = false;
    }
  }

  if (!snapshot.isNull()) {
    QSGTexture *tex = window()->createTextureFromImage(snapshot);
    if (tex)
      node->setTexture(tex);
  }

  node->setRect(boundingRect());
  return node;
}
